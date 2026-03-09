# ==============================================================================
# 04_structural_models_nestedness.R
#
# ATC archetype models — estimación sobre el dataset completo
#
# Tres arquetipos ATC basados en skill-complexity (cs) y dominio:
#   SC_Specialized    (referencia): Cognitivo, cs < mediana(cs | Cognitivo)
#   SC_Scaffolding:                 Cognitivo, cs >= mediana(cs | Cognitivo)
#   Physical_Terminal:              Físico (todos)
#
# Fórmula (ambos paneles):
#   diffusion ~ (wage_up + wage_down + up_dummy + structural_distance):atc_archetype
#
# Estrategia de RAM (igual que 03_structural_models.R):
#   - Solo fixest + data.table + ggplot2, sin tidyverse
#   - lean = TRUE + mem.clean = TRUE en todos los feglm
#   - nthreads = 2 para limitar RAM paralela durante el demeaning iterativo
#   - Secuencial: estimar → saveRDS → rm() → gc() → siguiente modelo
#   - Coeficientes extraídos antes de borrar el objeto del modelo
#
# Outputs (output/output_main/):
#   models/m_fe_source_nestedness.rds   Modelo Panel A (lean)
#   models/m_fe_target_nestedness.rds   Modelo Panel B (lean)
#   tables/coefs_nestedness.csv         Tabla de coeficientes (long format)
#   figs/fig_main_atc_nestedness.pdf    Figura 2x3 arquetipos
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------------------------
gc()
library(fixest)
library(ggplot2)
library(data.table)
source("R/utils.R")   # extract_coefs_archetype()

out_models <- file.path("output", "output_main", "models")
out_tables <- file.path("output", "output_main", "tables")
out_figs   <- file.path("output", "output_main", "figs")
for (d in c(out_models, out_tables, out_figs))
  dir.create(d, showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------------------------
# 1. Cargar datos — dataset completo, columnas mínimas
# ------------------------------------------------------------------------------
if (file.exists("R/99_paths_local.R")) source("R/99_paths_local.R")
dt_path <- if (exists("PATH_TRIADIC")) PATH_TRIADIC else "dt_con_cs_nestedness.rds"
if (!file.exists(dt_path))
  stop("Archivo de datos no encontrado: ", dt_path,
       "\nConfigura PATH_TRIADIC en R/99_paths_local.R")

message("Cargando datos desde: ", dt_path)
dt_model <- readRDS(dt_path)
if (!is.data.table(dt_model)) dt_model <- as.data.table(dt_model)
gc()

# Columnas necesarias — cs se necesita para atc_archetype, wage_gap para las
# variables direccionales; ambos se eliminan en cuanto ya no hacen falta.
needed <- c("diffusion", "wage_gap", "structural_distance",
            "domain", "source", "target", "skill_name", "cs")
drop   <- setdiff(names(dt_model), needed)
if (length(drop)) dt_model[, (drop) := NULL]
gc(); gc()

message(sprintf(
  ">>> Dataset: %s filas | tasa de difusión: %.2f%%",
  format(nrow(dt_model), big.mark = ","),
  mean(dt_model$diffusion) * 100
))

# ------------------------------------------------------------------------------
# 2. Construir arquetipo ATC (usa cs + domain) → eliminar cs
# ------------------------------------------------------------------------------
cs_threshold <- dt_model[domain == "Cognitive", median(cs, na.rm = TRUE)]
message("Umbral cs (mediana Cognitivo) = ", round(cs_threshold, 4))

dt_model[, atc_archetype := fcase(
  domain == "Cognitive" & cs >= cs_threshold, "SC_Scaffolding",
  domain == "Cognitive" & cs <  cs_threshold, "SC_Specialized",
  domain == "Physical",                        "Physical_Terminal"
)]
dt_model[, atc_archetype := factor(
  atc_archetype,
  levels = c("SC_Specialized", "SC_Scaffolding", "Physical_Terminal")
)]

# cs ya no se necesita — liberarlo inmediatamente
dt_model[, cs := NULL]
gc()

message("Distribución de arquetipos:")
print(dt_model[, .N, keyby = atc_archetype])

# ------------------------------------------------------------------------------
# 3. Variables direccionales de salario (usa wage_gap) → eliminar wage_gap
# ------------------------------------------------------------------------------
D_BAR <- mean(dt_model$structural_distance, na.rm = TRUE)
message("D_BAR = ", round(D_BAR, 4))

dt_model[, wage_up   := pmax(0,  wage_gap)]
dt_model[, wage_down := pmin(0,  wage_gap)]
dt_model[, up_dummy  := fifelse(wage_gap > 0, 1L, 0L)]

# wage_gap raw ya no se necesita — liberarlo
dt_model[, wage_gap := NULL]
gc(); gc()

message(sprintf(
  ">>> Datos listos para estimación: %s filas, %d columnas",
  format(nrow(dt_model), big.mark = ","),
  ncol(dt_model)
))
print(gc())

# ==============================================================================
# MODELOS — Se estiman de forma SECUENCIAL:
#   Panel A → saveRDS → rm → gc → Panel B → saveRDS → rm → gc
# Así nunca coexisten dos modelos grandes en RAM.
# ==============================================================================

# ------------------------------------------------------------------------------
# 4. Panel A — FE(source, skill_name)
# ------------------------------------------------------------------------------
message("\n>>> Panel A: feglm FE(source, skill_name) — dataset completo...")

m_fe_source <- feglm(
  diffusion ~ (wage_up + wage_down + up_dummy + structural_distance):atc_archetype,
  data      = dt_model,
  family    = binomial(link = "cloglog"),
  fixef     = c("source", "skill_name"),
  cluster   = c("source", "target", "skill_name"),
  lean      = TRUE, mem.clean = TRUE, nthreads = 2
)
gc(); gc()
summary(m_fe_source)

saveRDS(m_fe_source, file.path(out_models, "m_fe_source_nestedness.rds"))
message("  Guardado: models/m_fe_source_nestedness.rds")

# Extraer coeficientes ANTES de borrar el modelo
coefs_A <- extract_coefs_archetype(m_fe_source, "Panel A")

rm(m_fe_source); gc(); gc()
message("  Modelo Panel A eliminado de RAM.")

# ------------------------------------------------------------------------------
# 5. Panel B — FE(target, skill_name)
# ------------------------------------------------------------------------------
message("\n>>> Panel B: feglm FE(target, skill_name) — dataset completo...")

m_fe_target <- feglm(
  diffusion ~ (wage_up + wage_down + up_dummy + structural_distance):atc_archetype,
  data      = dt_model,
  family    = binomial(link = "cloglog"),
  fixef     = c("target", "skill_name"),
  cluster   = c("source", "target", "skill_name"),
  lean      = TRUE, mem.clean = TRUE, nthreads = 2
)
gc(); gc()
summary(m_fe_target)

saveRDS(m_fe_target, file.path(out_models, "m_fe_target_nestedness.rds"))
message("  Guardado: models/m_fe_target_nestedness.rds")

coefs_B <- extract_coefs_archetype(m_fe_target, "Panel B")

rm(m_fe_target); gc(); gc()
message("  Modelo Panel B eliminado de RAM.")

# Liberar datos — ya no hacen falta
rm(dt_model); gc(); gc()
message("  Datos eliminados de RAM.")

# ------------------------------------------------------------------------------
# 6. Tabla de coeficientes
# ------------------------------------------------------------------------------
message("\n>>> Tabla de coeficientes...")

coefs <- rbind(coefs_A, coefs_B)
print(coefs)

fwrite(coefs, file.path(out_tables, "coefs_nestedness.csv"))
message("  Guardado: tables/coefs_nestedness.csv")

# ==============================================================================
# 7. Figura — predictores lineales por arquetipo (2 x 3)
#
# Filas: Panel A / Panel B
# Columnas: SC_Specialized | SC_Scaffolding | Physical_Terminal
#
# Science Advances specs:
#   Ancho: 7.0 in (2-col); para 3 columnas se usa 10.5 in — confirmar con editor
#   Alto:  8.5 in  |  Fuente: Helvetica  |  Formato: cairo_pdf
# ==============================================================================
message("\n>>> Construyendo figura de arquetipos...")

# -- Paleta -------------------------------------------------------------------
PAL <- c(
  SC_Specialized    = "#2ABAB2",   # teal base — igual que Cognitive en 01/03
  SC_Scaffolding    = "#1a7a77",   # teal oscuro — mismo matiz, más saturado
  Physical_Terminal = "#CC4444"    # rojo — igual que Physical en 01/03
)

ARCH_LABS <- c(
  SC_Specialized    = "SC Specialized",
  SC_Scaffolding    = "SC Scaffolding",
  Physical_Terminal = "Physical Terminal"
)

PANEL_LABS <- c(
  "Panel A" = "(A)  Source FE",
  "Panel B" = "(B)  Target FE"
)

# -- Constantes de diseño -----------------------------------------------------
XLIM <- c(-2.2,  2.2)
YLIM <- c(-4.2,  2.6)
GAP0 <- 0.16
STEP <- 0.01

mm_from_pt <- function(pt) pt * 0.352778
PT_TICK <- 13; PT_TITLE <- 15; PT_STRIP <- 15; PT_ANNOT <- 12

# -- Extractor de coeficientes ------------------------------------------------
get_est <- function(coefs_dt, panel, arch, coef_name) {
  v <- coefs_dt[panel_short == panel & archetype == arch & coef == coef_name,
                estimate]
  if (length(v) == 0 || is.na(v[1])) 0 else v[1]
}

# -- Construir datos de la figura ---------------------------------------------
archetypes   <- c("SC_Specialized", "SC_Scaffolding", "Physical_Terminal")
panel_levels <- c("Panel A", "Panel B")

build_panel <- function(coefs_dt, panel_label) {
  x_left  <- seq(XLIM[1], -GAP0, by = STEP)
  x_right <- seq(GAP0,  XLIM[2], by = STEP)

  lines <- rbindlist(lapply(archetypes, function(arch) {
    b_up <- get_est(coefs_dt, panel_label, arch, "Theta_up")
    b_dn <- get_est(coefs_dt, panel_label, arch, "Theta_dn")
    b_k  <- get_est(coefs_dt, panel_label, arch, "kappa")
    b_d  <- get_est(coefs_dt, panel_label, arch, "delta")

    left  <- data.table(panel = panel_label, archetype = arch, side = "L",
                        x = x_left)
    left[,  y := b_dn * x + b_d * D_BAR]

    right <- data.table(panel = panel_label, archetype = arch, side = "R",
                        x = x_right)
    right[, y := b_up * x + b_k + b_d * D_BAR]

    rbind(left, right)
  }))

  jumps <- rbindlist(lapply(archetypes, function(arch) {
    b_k <- get_est(coefs_dt, panel_label, arch, "kappa")
    b_d <- get_est(coefs_dt, panel_label, arch, "delta")
    data.table(panel = panel_label, archetype = arch,
               x = 0, ymin = b_d * D_BAR, ymax = b_d * D_BAR + b_k)
  }))

  annots <- rbindlist(lapply(archetypes, function(arch) {
    data.table(
      panel     = panel_label,
      archetype = arch,
      label = sprintf(
        "\u03b2\u2191 = %+.3f\n\u03b2\u2193 = %+.3f\n\u03ba = %+.3f",
        get_est(coefs_dt, panel_label, arch, "Theta_up"),
        get_est(coefs_dt, panel_label, arch, "Theta_dn"),
        get_est(coefs_dt, panel_label, arch, "kappa")
      ),
      x = XLIM[1] + 0.12,
      y = YLIM[1] + 0.10
    )
  }))

  list(lines = lines, jumps = jumps, annots = annots)
}

pa <- build_panel(coefs, "Panel A")
pb <- build_panel(coefs, "Panel B")

all_lines  <- rbind(pa$lines,  pb$lines)
all_jumps  <- rbind(pa$jumps,  pb$jumps)
all_annots <- rbind(pa$annots, pb$annots)

all_lines[,  panel := factor(panel, levels = panel_levels)]
all_lines[,  archetype := factor(archetype, levels = archetypes)]
all_jumps[,  panel := factor(panel, levels = panel_levels)]
all_jumps[,  archetype := factor(archetype, levels = archetypes)]
all_annots[, panel := factor(panel, levels = panel_levels)]
all_annots[, archetype := factor(archetype, levels = archetypes)]

# -- Tema SciAdv --------------------------------------------------------------
theme_sciadv <- theme_classic(base_size = PT_TICK, base_family = "Helvetica") +
  theme(
    strip.text        = element_text(size = PT_STRIP, face = "plain",
                                     margin = margin(b = 4, t = 4)),
    strip.background  = element_blank(),
    axis.title.x      = element_text(size = PT_TITLE, margin = margin(t = 5)),
    axis.title.y      = element_text(size = PT_TITLE, margin = margin(r = 5)),
    axis.text         = element_text(size = PT_TICK),
    axis.ticks        = element_line(linewidth = 0.3, colour = "grey30"),
    axis.ticks.length = unit(2, "pt"),
    panel.grid        = element_blank(),
    panel.border      = element_rect(colour = "grey30", linewidth = 0.4,
                                     fill = NA),
    panel.background  = element_rect(fill = "white", colour = NA),
    panel.spacing.x   = unit(6, "pt"),
    panel.spacing.y   = unit(8, "pt"),
    plot.margin       = margin(t = 6, r = 8, b = 6, l = 6, unit = "pt"),
    legend.position   = "none"
  )

# -- Plot ---------------------------------------------------------------------
fig_nest <- ggplot() +

  geom_line(
    data = all_lines,
    aes(x = x, y = y, colour = archetype,
        group = interaction(panel, archetype, side)),
    linewidth = 1.2, lineend = "butt"
  ) +

  geom_segment(
    data = all_jumps,
    aes(x = x, xend = x, y = ymin, yend = ymax, colour = archetype),
    linewidth = 0.7
  ) +

  geom_vline(xintercept = 0, linetype = "dotted",
             linewidth = 0.25, colour = "grey70") +
  geom_hline(yintercept = 0, linetype = "dotted",
             linewidth = 0.25, colour = "grey70") +

  geom_text(
    data = all_annots,
    aes(x = x, y = y, label = label),
    family = "Helvetica", size = mm_from_pt(PT_ANNOT),
    hjust = 0, vjust = 0, lineheight = 1.15, colour = "grey20"
  ) +

  facet_grid(
    panel ~ archetype,
    labeller = labeller(archetype = ARCH_LABS, panel = PANEL_LABS)
  ) +

  scale_colour_manual(values = PAL) +

  scale_x_continuous(
    breaks = seq(-2, 2, by = 1),
    expand = expansion(mult = 0, add = 0.05)
  ) +
  scale_y_continuous(
    breaks = seq(-3, 2, by = 1),
    expand = expansion(mult = 0, add = 0.10)
  ) +

  coord_cartesian(xlim = XLIM, ylim = YLIM, clip = "on") +

  labs(
    x = "Signed log wage gap (target \u2212 source)",
    y = "Linear predictor (cloglog scale)"
  ) +

  theme_sciadv

# -- Guardar ------------------------------------------------------------------
# Ancho 10.5 in para acomodar 3 columnas de facetas;
# verificar límites con el editor de Science Advances antes de enviar.
ggsave(
  file.path(out_figs, "fig_main_atc_nestedness.pdf"),
  fig_nest, width = 10.5, height = 11.0, units = "in", device = cairo_pdf
)
message("  Guardado: figs/fig_main_atc_nestedness.pdf  [10.5 x 11.0 in]")

message("\n>>> 04_structural_models_nestedness.R completo.")
