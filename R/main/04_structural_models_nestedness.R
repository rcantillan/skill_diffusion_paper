library(fixest)
library(data.table)
library(tidyverse)


# ============================================================
# 0) Mantener sólo lo necesario (incluye nestedness_cat)
# ============================================================
dt <- readRDS("dt_con_cs_nestedness.rds")

# (si dt ya es data.table, esta parte igual funciona)
dt <- dt[, .(diffusion, wage_gap, structural_distance,
             domain, source, target, skill_name, cs)]

#dt <- dt_con_cs_nestedness; rm(dt_con_cs_nestedness)
gc();gc();gc()
setDT(dt)

# Asegurar tipos
dt[, domain := factor(domain)]
#dt[, nestedness_cat := factor(nestedness_cat)]   # levels: "Nested", "Un-nested" (según tu tabla)

# (opcional) fijar referencia
# dt[, nestedness_cat := relevel(nestedness_cat, ref = "Nested")]
# dt[, domain := relevel(domain, ref = "Cognitive")]

# ============================================================
# 1) Variables direccionales + dummy up
# ============================================================
dt[, wage_up   := pmax(0, wage_gap)]
dt[, wage_down := pmin(0, wage_gap)]
dt[, up_dummy  := fifelse(wage_gap > 0, 1L, 0L)]

gc(); gc(); gc()


# 1. Definir el umbral de 'Scaffolding' basado en el dominio Cognitivo
# (Ya que en el Físico no hay masa crítica para esta categoría)
cs_threshold <- dt[domain == "Cognitive", median(cs, na.rm = TRUE)]

# 2. Crear la variable de Arquetipos ATC
dt[, atc_archetype := fcase(
  domain == "Cognitive" & cs >= cs_threshold, "SC_Scaffolding",
  domain == "Cognitive" & cs <  cs_threshold, "SC_Specialized",
  domain == "Physical",                       "Physical_Terminal"
)]

# Convertir a factor y definir "SC_Specialized" como base para comparar 
# contra el "techo" (Scaffolding) y el "suelo" (Physical)
dt[, atc_archetype := factor(atc_archetype, 
                             levels = c("SC_Specialized", "SC_Scaffolding", "Physical_Terminal"))]

# ============================================================
# MODELO 1: "RAW" (sin FE) pero con nestedness_cat
#   Pendientes por (domain × nestedness_cat)
# ============================================================
m_raw_atc_nested <- feglm(
  diffusion ~ 
    # Interacción triple implícita: Gap * Arquetipo
    (wage_up + wage_down + structural_distance) * atc_archetype, 
  data = sample_frac(dt, .5),
  family = binomial(link = "cloglog"),
  cluster = c("source", "target", "skill_name"),
  mem.clean = TRUE, lean = TRUE, nthreads = 0
)

summary(m_raw_atc_nested)
gc(); gc(); gc()

# Crear la variable dummy de trayectoria ascendente
#dt[, up_dummy := as.integer(wage_up > 0)]

# Modelo con Salto Discreto (Interacción Triple)
m_fe_source_jump <- feglm(
  diffusion ~ 
    (wage_up + wage_down + up_dummy + structural_distance) : atc_archetype, 
  data = sample_frac(dt, .5),
  family = binomial(link = "cloglog"),
  fixef = c("source", "skill_name"),
  cluster = c("source", "target", "skill_name"),
  nthreads = 0
)
gc(); gc(); gc()
summary(m_fe_source_jump)

m_fe_target_jump <- feglm(
  diffusion ~ 
    (wage_up + wage_down + up_dummy + structural_distance) : atc_archetype, 
  data = sample_frac(dt, .5),
  family = binomial(link = "cloglog"),
  fixef = c("target", "skill_name"),
  cluster = c("source", "target", "skill_name"),
  nthreads = 0
)
gc(); gc(); gc()
summary(m_fe_target_jump)

# ============================================================
# ATC FIGURE — SINGLE MODEL (m_fe_source_nested)
#   2 panels: domain
#   Lines: Nested vs Un-nested (distinct colors) + LEGEND at bottom
#   Text: includes Nested/Un-nested + betas, colored to match
# ============================================================

library(data.table)
library(ggplot2)
library(sysfonts)
library(showtext)

font_add_google("Inconsolata", "inconsolata")
font_add_google("Noto Sans", "noto")
showtext_auto(TRUE)

theme_scd <- theme_minimal(base_size = 23) +
  theme(
    text = element_text(family = "inconsolata"),
    strip.text = element_text(face = "bold", size = 22, margin = margin(b = 10)),
    axis.title.x = element_text(size = 20, margin = margin(t = 18)),
    axis.title.y = element_text(size = 20, margin = margin(r = 18)),
    axis.text = element_text(size = 15),
    
    # legend abajo, simple
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    legend.margin = margin(t = 6, b = 0),
    legend.box.margin = margin(t = 6),
    
    panel.grid.minor = element_blank(),
    panel.spacing = unit(2.2, "lines"),
    plot.margin = margin(18, 28, 18, 28),
    panel.border = element_rect(color = "black", linewidth = 0.9, fill = NA),
    panel.background = element_blank()
  )

stopifnot(exists("dt"), exists("m_fe_source_nested"))
setDT(dt)

# ----------------------------
# Settings
# ----------------------------
xlim_use <- c(-2.2, 2.2)
step_x   <- 0.01
gap0     <- 0.16
ylim_use <- c(-3.6, 2.0)

domains <- c("Cognitive","Physical")
nests   <- c("Nested","Un-nested")

# two colors PER domain (so legend still works, but we need 4 actual hexes)
# We'll map color to an auxiliary key group4, and create a *manual legend* using guides().
cols4 <- c(
  "Cognitive|Nested"    = "#1f3a63",
  "Cognitive|Un-nested" = "#4f80c1",
  "Physical|Nested"     = "#d7263d",
  "Physical|Un-nested"  = "#ff7b89"
)

# to build a legend that says Nested vs Un-nested, we use override.aes (two entries)
# (see guides() below)

# ----------------------------
# Dbar
# ----------------------------
Dbar <- mean(dt$structural_distance, na.rm = TRUE)

# ----------------------------
# Robust coef extractor
# ----------------------------
get_coef <- function(model, term){
  b <- coef(model)
  if (term %in% names(b)) return(unname(b[[term]]))
  parts <- strsplit(term, ":", fixed = TRUE)[[1]]
  if(length(parts) >= 2){
    for(i in seq_along(parts)){
      for(j in seq_along(parts)){
        if(i < j){
          alt <- parts
          tmp <- alt[i]; alt[i] <- alt[j]; alt[j] <- tmp
          alt_term <- paste0(alt, collapse=":")
          if (alt_term %in% names(b)) return(unname(b[[alt_term]]))
        }
      }
    }
  }
  0
}

make_key <- function(domain, nest) paste(domain, nest, sep="|")

extract_bygroup <- function(model, domains, nests){
  out <- list(b_up=list(), b_down=list(), b_dummy=list(), b_D=list())
  for(d in domains){
    for(n in nests){
      k <- make_key(d,n)
      out$b_up[[k]]    <- get_coef(model, paste0("wage_up:domain", d, ":nestedness_cat", n))
      out$b_down[[k]]  <- get_coef(model, paste0("wage_down:domain", d, ":nestedness_cat", n))
      out$b_dummy[[k]] <- get_coef(model, paste0("up_dummy:domain", d, ":nestedness_cat", n))
      out$b_D[[k]]     <- get_coef(model, paste0("structural_distance:domain", d, ":nestedness_cat", n))
    }
  }
  out
}

bb <- extract_bygroup(m_fe_source_nested, domains, nests)

# ----------------------------
# Build piecewise segments + jump
# ----------------------------
x_left  <- seq(xlim_use[1], -gap0, by = step_x)
x_right <- seq(gap0, xlim_use[2],  by = step_x)

make_seg <- function(domain, nest, x, bb){
  k <- make_key(domain, nest)
  out <- data.table(domain = domain, nestedness_cat = nest, wage_gap = x)
  out[, group4 := paste(domain, nestedness_cat, sep="|")]
  out[, `:=`(
    wage_up   = pmax(0, wage_gap),
    wage_down = pmin(0, wage_gap),
    up_dummy  = as.integer(wage_gap > 0)
  )]
  out[, y :=
        bb$b_up[[k]]    * wage_up +
        bb$b_down[[k]]  * wage_down +
        bb$b_dummy[[k]] * up_dummy +
        bb$b_D[[k]]     * Dbar
  ]
  out
}

jump_at_zero <- function(domain, nest, bb){
  k <- make_key(domain, nest)
  y0m <- bb$b_D[[k]] * Dbar
  y0p <- bb$b_D[[k]] * Dbar + bb$b_dummy[[k]]
  data.table(domain = domain, nestedness_cat = nest,
             group4 = paste(domain, nest, sep="|"),
             x = 0, y0m = y0m, y0p = y0p)
}

line_left <- rbindlist(lapply(domains, function(d){
  rbindlist(lapply(nests, function(n) make_seg(d, n, x_left, bb)))
}))
line_right <- rbindlist(lapply(domains, function(d){
  rbindlist(lapply(nests, function(n) make_seg(d, n, x_right, bb)))
}))
jump_dt <- rbindlist(lapply(domains, function(d){
  rbindlist(lapply(nests, function(n) jump_at_zero(d, n, bb)))
}))

line_left[,  domain := factor(domain, levels = domains)]
line_right[, domain := factor(domain, levels = domains)]
jump_dt[,    domain := factor(domain, levels = domains)]

# ----------------------------
# Labels: include Nested/Un-nested + betas, colored to match line
# ----------------------------
labs <- rbindlist(lapply(domains, function(d){
  rbindlist(lapply(nests, function(n){
    k <- make_key(d,n)
    data.table(
      domain = d,
      nestedness_cat = n,
      group4 = paste(d, n, sep="|"),
      beta_up = bb$b_up[[k]],
      beta_down = bb$b_down[[k]]
    )
  }))
}))
labs[, domain := factor(domain, levels = domains)]

labs[, xL := xlim_use[1] + 0.20]
labs[, yB := ylim_use[1] + fifelse(nestedness_cat == "Nested", 0.25, 0.95)]

labs[, lab_block := sprintf("%s\nβ_up = %+.3f\nβ_down = %+.3f",
                            nestedness_cat, beta_up, beta_down)]

# ----------------------------
# Plot
# ----------------------------
p <- ggplot() +
  geom_line(
    data = line_left,
    aes(x = wage_gap, y = y, color = group4,
        group = interaction(domain, nestedness_cat)),
    linewidth = 3.2,
    lineend = "butt"
  ) +
  geom_line(
    data = line_right,
    aes(x = wage_gap, y = y, color = group4,
        group = interaction(domain, nestedness_cat)),
    linewidth = 3.2,
    lineend = "butt"
  ) +
  geom_segment(
    data = jump_dt,
    aes(x = x, xend = x, y = y0m, yend = y0p, group = group4),
    linewidth = 2.1,
    color = "black"
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 0.65) +
  facet_grid(. ~ domain) +
  scale_color_manual(
    values = cols4,
    # manual legend labels (we will override aesthetics to show just 2 legend entries)
    breaks = c("Cognitive|Nested","Cognitive|Un-nested"),
    labels = c("Nested","Un-nested")
  ) +
  guides(
    color = guide_legend(
      override.aes = list(
        # show a clean legend with two lines
        linewidth = c(3.2, 3.2),
        linetype  = c("solid","solid"),
        color     = c(cols4["Cognitive|Nested"], cols4["Cognitive|Un-nested"])
      )
    )
  ) +
  coord_cartesian(xlim = xlim_use, ylim = ylim_use, clip = "off") +
  geom_text(
    data = labs,
    aes(x = xL, y = yB, label = lab_block, color = group4),
    family = "noto",
    size = 4.6,
    hjust = 0, vjust = 0,
    lineheight = 1.05
  ) +
  labs(
    x = "Occupational Distance (Target Log-Wage − Source Log-Wage)",
    y = "Linear Predictor (cloglog scale)"
  ) +
  theme_scd

print(p)



# ============================================================
# ATC FIGURE: ASYMMETRIC STRUCTURE WITH DISCONTINUITY (2x3)
# Version: English / Publication Ready
# ============================================================

library(data.table)
library(ggplot2)
library(sysfonts)
library(showtext)

# 1. Load fonts for robust symbol rendering
font_add_google("Inconsolata", "inconsolata")
font_add_google("Noto Sans", "noto")
showtext_auto(TRUE)

# 2. Define SCD Aesthetics
theme_scd <- theme_minimal(base_size = 20) +
  theme(
    text = element_text(family = "inconsolata"),
    strip.text = element_text(face = "bold", size = 15),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14),
    panel.border = element_rect(color = "black", linewidth = 0.8, fill = NA),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.5, "lines"),
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 22),
    plot.subtitle = element_text(size = 16, color = "gray30")
  )

# Professional Palette
cols_atc <- c(
  "Socio-Cognitive (Medium Nestedness)" = "#5da5da", 
  "Socio-Cognitive (High Nestedness)"   = "#1f3a63", 
  "Physical (Low Nestedness / Terminal)" = "#d7263d"
)

# ----------------------------
# Simulation Settings
# ----------------------------
xlim_use <- c(-2.0, 2.0)
ylim_use <- c(-3.5, 2.5) 
gap_visual <- 0.04       
Dbar <- mean(dt$structural_distance, na.rm = TRUE)

# ----------------------------
# Coefficient Extractor
# ----------------------------
get_atc_params <- function(model, archetype) {
  b <- coef(model)
  suffix <- paste0("atc_archetype", archetype)
  list(
    b_up    = b[paste0("wage_up:", suffix)],
    b_down  = b[paste0("wage_down:", suffix)],
    b_dummy = b[paste0("up_dummy:", suffix)],
    b_dist  = b[paste0("structural_distance:", suffix)]
  )
}

# ----------------------------
# Data Preparation
# ----------------------------
prepare_plot_data <- function(model, label) {
  # Mapping Technical Names to Publication Labels
  archetypes <- c("SC_Specialized", "SC_Scaffolding", "Physical_Terminal")
  names(archetypes) <- c("Socio-Cognitive (Medium Nestedness)", 
                         "Socio-Cognitive (High Nestedness)", 
                         "Physical (Low Nestedness / Terminal)")
  
  lines_dt <- rbindlist(lapply(names(archetypes), function(pub_name) {
    tech_name <- archetypes[pub_name]
    p <- get_atc_params(model, tech_name)
    
    # Left Side (Downwards: x < 0)
    left <- data.table(wage_gap = seq(xlim_use[1], -gap_visual, length.out = 100))
    left[, y := p$b_down * wage_gap + p$b_dist * Dbar]
    left[, side := "down"]
    
    # Right Side (Upwards: x > 0)
    right <- data.table(wage_gap = seq(gap_visual, xlim_use[2], length.out = 100))
    right[, y := p$b_up * wage_gap + p$b_dummy + p$b_dist * Dbar]
    right[, side := "up"]
    
    dt_a <- rbind(left, right)
    dt_a[, `:=`(archetype = pub_name, model_name = label, b_up = p$b_up, b_down = p$b_down, b_dummy = p$b_dummy)]
    dt_a
  }))
  
  jumps_dt <- rbindlist(lapply(names(archetypes), function(pub_name) {
    tech_name <- archetypes[pub_name]
    p <- get_atc_params(model, tech_name)
    data.table(
      archetype = pub_name, model_name = label,
      x = 0,
      y_start = p$b_dist * Dbar,
      y_end   = p$b_dummy + p$b_dist * Dbar
    )
  }))
  
  list(lines = lines_dt, jumps = jumps_dt)
}

# Process Models
data_A <- prepare_plot_data(m_fe_source_jump, "Panel A: FE(source + skill)")
data_B <- prepare_plot_data(m_fe_target_jump, "Panel B: FE(target + skill)")

all_lines <- rbind(data_A$lines, data_B$lines)
all_jumps <- rbind(data_A$jumps, data_B$jumps)

# Order Panels
pub_levels <- c("Socio-Cognitive (Medium Nestedness)", "Socio-Cognitive (High Nestedness)", "Physical (Low Nestedness / Terminal)")
all_lines[, archetype := factor(archetype, levels = pub_levels)]
all_jumps[, archetype := factor(archetype, levels = pub_levels)]

# ----------------------------
# Robust Annotations (Unicode)
# ----------------------------
ann_data <- all_lines[, .(
  lab = sprintf("\u03b2_up: %.2f\n\u03b2_dn: %.2f\n\u0394_0: %.2f", 
                first(b_up), first(b_down), first(b_dummy))
), by = .(model_name, archetype)]

# ----------------------------
# Final Visualization
# ----------------------------
p_final <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray70") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
  
  # Slopes
  geom_line(data = all_lines, 
            aes(x = wage_gap, y = y, color = archetype, group = interaction(archetype, side)),
            linewidth = 2.8, lineend = "butt") +
  
  # Vertical Barrier (The Jump)
  geom_segment(data = all_jumps, 
               aes(x = x, xend = x, y = y_start, yend = y_end, color = archetype),
               linewidth = 1.6, linetype = "solid") +
  
  facet_grid(model_name ~ archetype) +
  
  # Text with Noto font for Symbols
  geom_text(data = ann_data, 
            aes(x = xlim_use[1] + 0.1, y = ylim_use[1] + 0.3, label = lab),
            family = "noto", size = 4.2, hjust = 0, vjust = 0, color = "black", lineheight = 0.9) +
  
  scale_color_manual(values = cols_atc) +
  coord_cartesian(xlim = xlim_use, ylim = ylim_use) +
  labs(
    x = "Occupational Wage Gap (Target - Source)",
    y = "Linear Predictor (cloglog scale)"
    #title = "Asymmetric Trajectory Channeling (ATC) and Vertical Barriers",
    #subtitle = "Diffusion probability conditioned by structural position and wage directionality"
  ) +
  theme_scd

print(p_final)

# Save high-res for the paper
#ggsave("fig_atc_publication_ready_EN.png", plot = p_final, width = 16, height = 9, dpi = 300)