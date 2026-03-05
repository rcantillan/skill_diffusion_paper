# ============================================================
# Figs. S3–S4 — Skill-level directional profiles
# CÓDIGO COMPLETO Y ACTUALIZADO
# ============================================================

library(data.table)
library(ggplot2)

# ── 1. Cargar datos completos y filtrar adoptadas ─────────────────────────────
# Optional: load local paths
if (file.exists("R/99_paths_local.R")) source("R/99_paths_local.R")

if (!exists("PATH_DYADS_V12")) {
  stop("PATH_DYADS_V12 is not defined. ",
       "Copy R/99_paths_local.R.example to R/99_paths_local.R ",
       "and set PATH_DYADS_V12 to the local path of the v12 dyads file.")
}
DYADS_PATH <- PATH_DYADS_V12

dt_full <- readRDS(DYADS_PATH)
setDT(dt_full)

dt_adopted <- dt_full[
  diffusion == 1,
  .(skill_name, domain, s_wage, t_wage)
]
rm(dt_full); gc(); gc(); gc()
cat("Filas adoptadas:", nrow(dt_adopted), "\n")

# ── 2. Mergear cs ─────────────────────────────────────────────────────────────
dt_cs     <- readRDS("dt_con_cs_nestedness.rds")
setDT(dt_cs)
cs_lookup <- unique(dt_cs[, .(skill_name, domain, cs)])
rm(dt_cs); gc(); gc(); gc()

dt_adopted <- merge(dt_adopted, cs_lookup,
                    by = c("skill_name", "domain"),
                    all.x = TRUE)
rm(cs_lookup); gc(); gc()

# ── 3. Quintiles y shift ──────────────────────────────────────────────────────
all_wages   <- unique(c(dt_adopted$s_wage, dt_adopted$t_wage))
wage_breaks <- quantile(all_wages, probs = seq(0, 1, 0.2), na.rm = TRUE)
rm(all_wages); gc()

dt_adopted[, s_q := as.integer(cut(s_wage, breaks = wage_breaks,
                                    labels = 1:5, include.lowest = TRUE))]
dt_adopted[, t_q := as.integer(cut(t_wage, breaks = wage_breaks,
                                    labels = 1:5, include.lowest = TRUE))]
dt_adopted[, quintile_shift := t_q - s_q]
dt_adopted[, c("s_wage", "t_wage", "s_q", "t_q") := NULL]
gc(); gc()

# ── 4. Agregación por skill ───────────────────────────────────────────────────
skill_shifts <- dt_adopted[, .(
  mean_shift = mean(quintile_shift, na.rm = TRUE),
  se_shift   = sd(quintile_shift,   na.rm = TRUE) / sqrt(.N),
  n_adopted  = .N
), by = .(skill_name, domain, cs)]

skill_shifts[, ci_lo := mean_shift - 1.96 * se_shift]
skill_shifts[, ci_hi := mean_shift + 1.96 * se_shift]

rm(dt_adopted); gc(); gc(); gc()

# Chequeo
cat("\nDistribución de shifts por dominio:\n")
print(skill_shifts[, .(
  pct_positive = round(mean(mean_shift > 0) * 100, 1),
  mean_shift   = round(mean(mean_shift), 3),
  min          = round(min(mean_shift), 3),
  max          = round(max(mean_shift), 3)
), by = domain])

# ── 5. Archetypes ─────────────────────────────────────────────────────────────
threshold_cog <- skill_shifts[domain == "Cognitive", median(cs, na.rm = TRUE)]
threshold_phy <- skill_shifts[domain == "Physical",  median(cs, na.rm = TRUE)]

skill_shifts[, archetype := fcase(
  domain == "Cognitive" & cs >= threshold_cog, "SC_Scaffolding",
  domain == "Cognitive" & cs <  threshold_cog, "SC_Specialized",
  domain == "Physical"  & cs <  threshold_phy, "Physical_Terminal",
  domain == "Physical"  & cs >= threshold_phy, "Physical_HN"
)]

# ── 6. Paleta ─────────────────────────────────────────────────────────────────
arch_colors <- c(
  "SC_Scaffolding"    = "#1A8F8B",
  "SC_Specialized"    = "#88D4D1",
  "Physical_Terminal" = "#CC4444",
  "Physical_HN"       = "#E8A0A0"
)
arch_shapes <- c(
  "SC_Scaffolding"    = 21,
  "SC_Specialized"    = 21,
  "Physical_Terminal" = 21,
  "Physical_HN"       = 24
)

# ── 7. Función de plot ────────────────────────────────────────────────────────
make_shift_plot <- function(dat, x_lo = -1.6, x_hi = 1.6) {

  dat <- copy(dat[order(mean_shift)])
  dat[, skill_name := factor(skill_name, levels = skill_name)]

  n_skills      <- nrow(dat)
  lateral_band  <- 0.25
  archs_present <- unique(dat$archetype)
  colors_use    <- arch_colors[archs_present]
  shapes_use    <- arch_shapes[archs_present]

  ggplot(dat, aes(x = mean_shift, y = skill_name)) +

    annotate("rect",
             xmin = lateral_band, xmax = Inf,
             ymin = -Inf, ymax = Inf,
             fill = "#2ABAB2", alpha = 0.07) +
    annotate("rect",
             xmin = -Inf, xmax = -lateral_band,
             ymin = -Inf, ymax = Inf,
             fill = "#CC4444", alpha = 0.07) +
    annotate("rect",
             xmin = -lateral_band, xmax = lateral_band,
             ymin = -Inf, ymax = Inf,
             fill = "#DDDDDD", alpha = 0.35) +

    geom_vline(xintercept = 0,
               linewidth = 0.55, color = "grey20") +
    geom_vline(xintercept = c(-lateral_band, lateral_band),
               linewidth = 0.3, color = "grey55", linetype = "dotted") +

    geom_errorbarh(
      aes(xmin = ci_lo, xmax = ci_hi),
      height    = 0.25,
      linewidth = 0.30,
      color     = "grey35"
    ) +

    geom_point(
      aes(fill = archetype, shape = archetype),
      size   = 2.4,
      color  = "white",
      stroke = 0.3
    ) +

    annotate("text",
             x = x_hi - 0.05, y = n_skills + 0.7,
             label = "Upward", hjust = 1, vjust = 0,
             size = 2.6, color = "#1A8F8B", fontface = "italic") +
    annotate("text",
             x = x_lo + 0.05, y = n_skills + 0.7,
             label = "Downward", hjust = 0, vjust = 0,
             size = 2.6, color = "#AA3333", fontface = "italic") +
    annotate("text",
             x = 0, y = n_skills + 0.7,
             label = "Lateral", hjust = 0.5, vjust = 0,
             size = 2.6, color = "grey45", fontface = "italic") +

    scale_fill_manual(values = colors_use, name = NULL) +
    scale_shape_manual(values = shapes_use, name = NULL) +

    scale_x_continuous(
      limits = c(x_lo, x_hi),
      breaks = seq(-1.5, 1.5, 0.5),
      labels = function(x) ifelse(x > 0, paste0("+", x), as.character(x)),
      expand = c(0, 0)
    ) +
    scale_y_discrete(expand = expansion(add = c(0.5, 2.0))) +

    labs(
      x = "Mean wage-quintile shift at destination (conditional on diffusion)",
      y = NULL
    ) +

    theme_classic(base_size = 9) +
    theme(
      axis.text.y          = element_text(size  = 7, hjust = 1,
                                          color = "grey15"),
      axis.line.y          = element_blank(),
      axis.ticks.y         = element_blank(),
      axis.line.x          = element_line(linewidth = 0.4),
      axis.ticks.x         = element_line(linewidth = 0.4),
      axis.title.x         = element_text(size   = 8,
                                          margin = margin(t = 7)),
      panel.grid.major.x   = element_line(linewidth = 0.2,
                                          color     = "grey88"),
      legend.position      = "bottom",
      legend.direction     = "horizontal",
      legend.key.size      = unit(0.55, "cm"),
      legend.text          = element_text(size = 8),
      legend.background    = element_blank(),
      legend.margin        = margin(t = 4),
      plot.margin = margin(4, 50, 6, 6)
    )
}

# ── 8. Generar y guardar ──────────────────────────────────────────────────────
fig_s3 <- make_shift_plot(skill_shifts[domain == "Cognitive"])

ggsave("fig_s3_shifts_cognitive.pdf", fig_s3,
       width = 6.5, height = 10, device = cairo_pdf)
ggsave("fig_s3_shifts_cognitive.png", fig_s3,
       width = 6.5, height = 10, dpi = 300)

rm(fig_s3); gc(); gc()

fig_s4 <- make_shift_plot(skill_shifts[domain == "Physical"])

ggsave("fig_s4_shifts_physical.pdf", fig_s4,
       width = 6.5, height = 10, device = cairo_pdf)
ggsave("fig_s4_shifts_physical.png", fig_s4,
       width = 6.5, height = 10, dpi = 300)

rm(fig_s4, skill_shifts); gc(); gc(); gc()

cat("Listo. Figuras S3 y S4 guardadas.\n")
