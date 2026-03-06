
library(fixest)
library(tidyverse)
library(ggplot2)
library(data.table)
library(sysfonts)
library(showtext)
library(grid)
library(glmmTMB)
library(lme4)


# Consolidamos solo las columnas necesarias (Ahorro masivo de RAM)
dt <- dt %>% dplyr::select("diffusion", "wage_gap", "structural_distance", 
                           "domain", "source", "target", "skill_name")

setDT(dt)
# ==============================================================================
# MODELO ATC DEFINITIVO (17M OBSERVACIONES)
# ==============================================================================

# 1. Crear las variables de dirección (magnitudes positivas)
dt[, wage_up := pmax(0, wage_gap)]
dt[, wage_down := pmin(0, wage_gap)]

# agregar dummy de up = 1 o cero
dt[, up_dummy := fifelse(wage_gap > 0, 1L, 0L)]


# 2. Estimación con interacción anidada
# Usamos el operador ':' para que estime la pendiente DENTRO de cada grupo
m_raw_atc <- feglm(
  diffusion ~ 
  (wage_up + wage_down + structural_distance)*domain,
  data = dt,
  family = binomial(link = "cloglog"),
  #fixef = c("source", "target", "skill_name"),
  cluster = c("source", "target", "skill_name"),
  mem.clean = TRUE, nthreads = 0, lean = TRUE
)

gc(); gc(); gc()
# 3. Ver el resultado
summary(m_raw_atc)
gc(); gc(); gc()


# modelar masa ocupacion y de skill y posibles confounders directamente
m__fe_source <- feglm(
  diffusion ~ 
    (up_dummy + wage_up + wage_down + structural_distance):domain,
  data = dt,
  family = binomial(link = "cloglog"),
  fixef = c("source", "skill_name"),
  cluster = c("source", "target", "skill_name"),
  mem.clean = TRUE, nthreads = 0, lean = TRUE)

summary(m__fe_source)
gc(); gc(); gc()


m__fe_target <- feglm(
  diffusion ~ 
    (up_dummy + wage_up + wage_down + structural_distance):domain,
  data = dt,
  family = binomial(link = "cloglog"),
  fixef = c("target", "skill_name"),
  cluster = c("source", "target", "skill_name"),
  mem.clean = TRUE, nthreads = 0, lean = TRUE)

summary(m__fe_target)
gc(); gc(); gc()




# ============================================================
# ATC FIGURE — FE SOURCE vs FE TARGET (2x2)
#   Rows: model (A/B)   Cols: domain
#   Piecewise slopes + explicit discontinuity (Δ0)
#   Annotations (bottom-left): Δ0, β_up, β_down
#   Y-axis up to 2
#   Panel borders ON
#   NO empirical cloud points
#   FIX: Panel tags A/B visible (inside panels)
# ============================================================

library(data.table)
library(ggplot2)
library(sysfonts)
library(showtext)
library(grid)

font_add_google("Inconsolata", "inconsolata")
font_add_google("Noto Sans", "noto")
showtext_auto(TRUE)

theme_scd <- theme_minimal(base_size = 23) +
  theme(
    text = element_text(family = "inconsolata"),
    plot.title = element_text(face = "bold", size = 30, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 20, color = "gray30", margin = margin(b = 14)),
    strip.text = element_text(face = "bold", size = 22, margin = margin(b = 10)),
    axis.title.x = element_text(size = 20, margin = margin(t = 18)),
    axis.title.y = element_text(size = 20, margin = margin(r = 18)),
    axis.text = element_text(size = 15),
    strip.text.y.right = element_text(
      margin = margin(l = 12, r = 12)  # 👈 empuja el texto hacia afuera
    ),
    
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.spacing = unit(2.6, "lines"),
    plot.margin = margin(18, 26, 18, 26),
    plot.caption = element_text(
      size = 13, hjust = 0.5, color = "gray20",
      margin = margin(t = 18), lineheight = 1.15
    ),
    panel.border = element_rect(color = "black", linewidth = 0.9, fill = NA),
    panel.background = element_blank()
  )

stopifnot(exists("dt"), exists("m__fe_target"), exists("m__fe_source"))
setDT(dt)

# ----------------------------
# Settings
# ----------------------------
xlim_use <- c(-2.2, 2.2)
step_x   <- 0.01
gap0     <- 0.16
ylim_use <- c(-3.6, 2.0)

cols <- c(Cognitive = "#1f3a63", Physical = "#d7263d")

# ----------------------------
# Signed wage gap for plotting
# ----------------------------
if(!("wage_gap" %in% names(dt))){
  dt[, wage_gap_plot := wage_up + wage_down]
} else {
  dt[, wage_gap_plot := wage_gap]
}

# ----------------------------
# Robust coefficient extractor
# ----------------------------
get_coef <- function(model, term){
  b <- coef(model)
  if (term %in% names(b)) return(unname(b[[term]]))
  parts <- strsplit(term, ":", fixed = TRUE)[[1]]
  if (length(parts) == 2){
    alt <- paste0(parts[2], ":", parts[1])
    if (alt %in% names(b)) return(unname(b[[alt]]))
  }
  0
}

extract_bydomain <- function(model){
  list(
    b_up = c(
      Cognitive = get_coef(model, "wage_up:domainCognitive"),
      Physical  = get_coef(model, "wage_up:domainPhysical")
    ),
    b_down = c(
      Cognitive = get_coef(model, "wage_down:domainCognitive"),
      Physical  = get_coef(model, "wage_down:domainPhysical")
    ),
    b_dummy = c(
      Cognitive = get_coef(model, "up_dummy:domainCognitive"),
      Physical  = get_coef(model, "up_dummy:domainPhysical")
    ),
    b_D = c(
      Cognitive = get_coef(model, "structural_distance:domainCognitive"),
      Physical  = get_coef(model, "structural_distance:domainPhysical")
    )
  )
}

# ----------------------------
# Model-implied piecewise lines + explicit jump at x=0
# ----------------------------
Dbar <- mean(dt$structural_distance, na.rm = TRUE)

x_left  <- seq(xlim_use[1], -gap0, by = step_x)
x_right <- seq(gap0, xlim_use[2],  by = step_x)

make_seg <- function(domain, x, bb, model_name){
  out <- data.table(model = model_name, domain = domain, wage_gap = x)
  out[, `:=`(
    wage_up   = pmax(0, wage_gap),
    wage_down = pmin(0, wage_gap),
    up_dummy  = as.integer(wage_gap > 0)
  )]
  out[, y :=
        bb$b_up[domain]    * wage_up +
        bb$b_down[domain]  * wage_down +
        bb$b_dummy[domain] * up_dummy +
        bb$b_D[domain]     * Dbar
  ]
  out
}

jump_at_zero <- function(domain, bb, model_name){
  y0m <- bb$b_D[domain] * Dbar
  y0p <- bb$b_D[domain] * Dbar + bb$b_dummy[domain]
  data.table(model = model_name, domain = domain, x = 0, y0m = y0m, y0p = y0p)
}

build_lines <- function(model, model_name){
  bb <- extract_bydomain(model)
  lines_left <- rbind(
    make_seg("Cognitive", x_left,  bb, model_name),
    make_seg("Physical",  x_left,  bb, model_name)
  )
  lines_right <- rbind(
    make_seg("Cognitive", x_right, bb, model_name),
    make_seg("Physical",  x_right, bb, model_name)
  )
  jumps <- rbind(
    jump_at_zero("Cognitive", bb, model_name),
    jump_at_zero("Physical",  bb, model_name)
  )
  list(lines_left = lines_left, lines_right = lines_right, jumps = jumps, bb = bb)
}

# A (top) = FE(source + skill)
# B (bottom) = FE(target + skill)
L_A <- build_lines(m__fe_source, "FE(source + skill)")
L_B <- build_lines(m__fe_target, "FE(target + skill)")

line_left  <- rbind(L_A$lines_left,  L_B$lines_left)
line_right <- rbind(L_A$lines_right, L_B$lines_right)
jump_dt    <- rbind(L_A$jumps, L_B$jumps)

model_levels <- c("FE(source + skill)", "FE(target + skill)")
line_left[,  model := factor(model, levels = model_levels)]
line_right[, model := factor(model, levels = model_levels)]
jump_dt[,    model := factor(model, levels = model_levels)]

# ----------------------------
# Bottom-left annotations: Δ0, β_up, β_down
# ----------------------------
make_labs <- function(model_name, bb){
  labs <- data.table(
    model  = model_name,
    domain = c("Cognitive","Physical"),
    delta0    = bb$b_dummy[c("Cognitive","Physical")],
    beta_up   = bb$b_up[c("Cognitive","Physical")],
    beta_down = bb$b_down[c("Cognitive","Physical")]
  )
  labs[, lab_block := sprintf("β_up = %+.3f\nβ_down = %+.3f",
                              delta0, beta_up, beta_down)]
  labs
}

labs <- rbind(
  make_labs("FE(source + skill)", L_A$bb),
  make_labs("FE(target + skill)", L_B$bb)
)
labs[, model := factor(model, levels = model_levels)]

labs[, `:=`(
  xL = xlim_use[1] + 0.20,
  yB = ylim_use[1] + 0.25
)]

# ----------------------------
# Panel tags A/B (visible)
# Place inside top-left of EACH panel in the row (both domains)
# ----------------------------

panel_tags <- data.table(
  model  = factor(c("FE(source + skill)", "FE(target + skill)"), levels = model_levels),
  domain = "Cognitive",
  tag    = c("A", "B"),
  x      = xlim_use[1] - 0.60,   # outside panel (left)
  y      = ylim_use[2] - 0.10    # near top
)

# ----------------------------
# Caption
# ----------------------------
note_text <- paste(
  "Note: Solid lines are model-implied FE cloglog linear predictors.",
  "Discontinuity at x=0 is shown explicitly (Δ0 = β_dummy).",
  "Panel A: FE(source + skill). Panel B: FE(target + skill).",
  "Annotations report Δ0, β_up, and β_down."
)
note_text <- paste(strwrap(note_text, width = 118), collapse = "\n")

# ----------------------------
# Plot
# ----------------------------
p <- ggplot() +
  geom_line(
    data = line_left,
    aes(x = wage_gap, y = y, color = domain, group = interaction(model, domain)),
    linewidth = 3.3,
    lineend = "butt"
  ) +
  geom_line(
    data = line_right,
    aes(x = wage_gap, y = y, color = domain, group = interaction(model, domain)),
    linewidth = 3.3,
    lineend = "butt"
  ) +
  geom_segment(
    data = jump_dt,
    aes(x = x, xend = x, y = y0m, yend = y0p),
    linewidth = 2.2,
    linetype = "solid",
    color = "black"
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 0.65) +
  facet_grid(model ~ domain) +
  scale_color_manual(values = cols) +
  coord_cartesian(xlim = xlim_use, ylim = ylim_use, clip = "off") +
  geom_text(
    data = labs,
    aes(x = xL, y = yB, label = lab_block),
    family = "noto",
    size = 4.8,
    hjust = 0, vjust = 0,
    lineheight = 1.05
  ) +
  geom_text(
    data = panel_tags,
    aes(x = x, y = y, label = tag),
    inherit.aes = FALSE,
    family = "noto",
    fontface = "bold",
    size = 8,
    hjust = .5, vjust = .001
  ) +
  labs(
    #title = "Asymmetric Trajectory Channeling (ATC) — FE Decomposition",
    #subtitle = "Piecewise slopes plus an upward-entry discontinuity (Δ0) by domain.",
    x = "Occupational Distance (Target Log-Wage − Source Log-Wage)",
    y = "Linear Predictor (cloglog scale)"
    #caption = note_text
  ) +
  theme_scd


print(p)
