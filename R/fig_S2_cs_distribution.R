# =============================================================================
# fig_S2_cs_distribution.R
# Fig. S2 — Distribution of nestedness contributions (c_s) by skill domain
# =============================================================================

library(data.table)
library(ggplot2)

# ── 1. Cargar y preparar ──────────────────────────────────────────────────────
dt <- readRDS("dt_con_cs_nestedness.rds")
setDT(dt)

# Tabla de skills únicos (cs es invariante por skill, no por díada)
dt_skills <- unique(dt[, .(skill_name, domain, cs)])

# Etiquetas legibles para el plot
dt_skills[, domain_label := fcase(
  domain == "Cognitive", "Socio-Cognitive",
  domain == "Physical",  "Sensory/Physical"
)]

# Umbral real de clasificación: solo el cognitivo
threshold_cog <- dt_skills[domain == "Cognitive", median(cs, na.rm = TRUE)]

# Rango del eje x para los rectángulos de región
xmin_total <- dt_skills[, min(cs, na.rm = TRUE)] - 0.5
xmax_total <- dt_skills[, max(cs, na.rm = TRUE)] + 0.5

fig_s2 <- ggplot(dt_skills, aes(x = cs, fill = domain_label, color = domain_label)) +

  # Sombreado de regiones de archetype
  annotate("rect",
           xmin = xmin_total, xmax = threshold_cog,
           ymin = 0, ymax = Inf,
           fill = "#2ABAB2", alpha = 0.04) +
  annotate("rect",
           xmin = threshold_cog, xmax = xmax_total,
           ymin = 0, ymax = Inf,
           fill = "#2ABAB2", alpha = 0.04) +

  geom_density(alpha = 0.22, linewidth = 0.7) +

  # Frontera de clasificación cognitiva
  geom_vline(
    data = data.frame(xint = threshold_cog),
    aes(xintercept = xint),
    color     = "#1A8F8B",
    linetype  = "dashed",
    linewidth = 0.55,
    inherit.aes = FALSE
  ) +

  # Etiquetas
  annotate("text",
           x = threshold_cog - 0.2, y = 0.28,
           label = "SC_Specialized", hjust = 1,
           size = 3, color = "#1A8F8B", fontface = "italic") +
  annotate("text",
           x = threshold_cog + 0.2, y = 0.28,
           label = "SC_Scaffolding", hjust = 0,
           size = 3, color = "#1A8F8B", fontface = "italic") +

  annotate("segment",
           x = threshold_cog - 0.2, xend = threshold_cog - 1.8,
           y = 0.265, yend = 0.265,
           color = "#1A8F8B", linewidth = 0.35,
           arrow = arrow(length = unit(0.12, "cm"), type = "closed")) +
  annotate("segment",
           x = threshold_cog + 0.2, xend = threshold_cog + 1.8,
           y = 0.265, yend = 0.265,
           color = "#1A8F8B", linewidth = 0.35,
           arrow = arrow(length = unit(0.12, "cm"), type = "closed")) +

  annotate("text",
           x = -7, y = 0.27,
           label = "Physical_Terminal",
           hjust = 0.5, size = 3,
           color = "#555555", fontface = "italic") +

  scale_fill_manual(
    values = c("Socio-Cognitive"  = "#2ABAB2",
               "Sensory/Physical" = "#888888"),
    name = NULL
  ) +
  scale_color_manual(
    values = c("Socio-Cognitive"  = "#1A8F8B",
               "Sensory/Physical" = "#555555"),
    name = NULL
  ) +

  coord_cartesian(ylim = c(0, 0.30)) +

  labs(
    x = expression("Standardized nestedness contribution (" * italic(c)[s] * ")"),
    y = "Density"
  ) +

  theme_classic(base_size = 15) +
  theme(
    legend.position   = "bottom",
    legend.direction  = "horizontal",
    legend.background = element_blank(),
    legend.key.size   = unit(0.8, "lines"),
    legend.text       = element_text(size = 9),
    axis.line         = element_line(linewidth = 0.4),
    axis.ticks        = element_line(linewidth = 0.4),
    plot.margin       = margin(10, 12, 6, 8)
  ) +
  guides(
    fill  = guide_legend(override.aes = list(alpha = 0.4, color = NA)),
    color = "none"
  )

fig_s2

ggsave("fig_s2_cs_distribution.pdf", fig_s2,
       width = 5.5, height = 3.8, device = cairo_pdf)
ggsave("fig_s2_cs_distribution.png", fig_s2,
       width = 5.5, height = 3.8, dpi = 300)
