# =============================================================================
# DEPRECATED — This script is superseded by R/05_tablas_y_figuras.R
# which generates all three SI robustness figures (Tests i–iii).
# Kept for reference only. Do not use for final figure generation.
# =============================================================================

# =============================================================================
# fig_SI_test_ii_FINAL.R
# Figura 2 — Domain-label permutation test
# =============================================================================
# SOLUCIÓN FINAL: valores de baseline como LITERALES NUMÉRICOS.
# annotate() en algunas versiones de ggplot2 captura el argumento x como
# una promesa (lazy evaluation) y no fuerza su evaluación al construir el layer.
# Al usar un literal numérico no hay promesa: el valor ES el valor.
#
# Baselines Panel A (de output de diagnóstico, fijos para este paper):
#   Theta_up_Cog =  0.3365311
#   Theta_up_Phy = -0.8185439
#   Theta_dn_Cog =  0.1474137
#   Theta_dn_Phy = -0.1461882
#
# Correr en sesión limpia: Session -> Restart R
# =============================================================================

library(data.table)
library(ggplot2)
library(patchwork)

output_dir <- "output_SI_identity"

perm_ii <- readRDS(file.path(output_dir, "rds", "test_ii_domain_perm.rds"))

ATC_COEFS <- c("Theta_up_Cog", "Theta_dn_Cog", "Theta_up_Phy", "Theta_dn_Phy")
COL_A <- "#2E6EA6"
COL_B <- "#C0392B"
coef_labels <- c(
  Theta_up_Cog = "\u0398\u2191 Cognitive",
  Theta_dn_Cog = "\u0398\u2193 Cognitive",
  Theta_up_Phy = "\u0398\u2191 Physical",
  Theta_dn_Phy = "\u0398\u2193 Physical"
)

pd <- perm_ii[coef %in% ATC_COEFS]

th <- theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(),
        axis.title       = element_blank(),
        plot.title       = element_text(size = 9.5, face = "bold", hjust = 0.5))

# =============================================================================
# Panel A — baseline como geom_vline con data.frame literal
# geom_vline(data = data.frame(xi = LITERAL), aes(xintercept = xi))
# El data.frame evalúa el literal al construirse — sin promesas, sin variables.
# =============================================================================

pA_up_cog <- ggplot(pd[panel_short == "Panel A" & coef == "Theta_up_Cog"],
                    aes(x = estimate)) +
  geom_histogram(bins = 55, fill = COL_A, alpha = .78,
                 color = "white", linewidth = .12) +
  geom_vline(xintercept = 0, color = "gray55",
             linewidth = .4, linetype = "dotted") +
  geom_vline(data = data.frame(xi = 0.3365311),
             aes(xintercept = xi),
             color = "#111111", linewidth = .95,
             inherit.aes = FALSE) +
  ggtitle(coef_labels[["Theta_up_Cog"]]) + th

pA_up_phy <- ggplot(pd[panel_short == "Panel A" & coef == "Theta_up_Phy"],
                    aes(x = estimate)) +
  geom_histogram(bins = 55, fill = COL_A, alpha = .78,
                 color = "white", linewidth = .12) +
  geom_vline(xintercept = 0, color = "gray55",
             linewidth = .4, linetype = "dotted") +
  geom_vline(data = data.frame(xi = -0.8185439),
             aes(xintercept = xi),
             color = "#111111", linewidth = .95,
             inherit.aes = FALSE) +
  ggtitle(coef_labels[["Theta_up_Phy"]]) + th

pA_dn_cog <- ggplot(pd[panel_short == "Panel A" & coef == "Theta_dn_Cog"],
                    aes(x = estimate)) +
  geom_histogram(bins = 55, fill = COL_A, alpha = .78,
                 color = "white", linewidth = .12) +
  geom_vline(xintercept = 0, color = "gray55",
             linewidth = .4, linetype = "dotted") +
  geom_vline(data = data.frame(xi = 0.1474137),
             aes(xintercept = xi),
             color = "#111111", linewidth = .95,
             inherit.aes = FALSE) +
  ggtitle(coef_labels[["Theta_dn_Cog"]]) + th

pA_dn_phy <- ggplot(pd[panel_short == "Panel A" & coef == "Theta_dn_Phy"],
                    aes(x = estimate)) +
  geom_histogram(bins = 55, fill = COL_A, alpha = .78,
                 color = "white", linewidth = .12) +
  geom_vline(xintercept = 0, color = "gray55",
             linewidth = .4, linetype = "dotted") +
  geom_vline(data = data.frame(xi = -0.1461882),
             aes(xintercept = xi),
             color = "#111111", linewidth = .95,
             inherit.aes = FALSE) +
  ggtitle(coef_labels[["Theta_dn_Phy"]]) + th

# Verificación: el valor debe estar en el data del layer, no en params
cat("=== Verificación layers Panel A ===\n")
cat("pA_up_cog xi:", pA_up_cog$layers[[3]]$data$xi, "\n")
cat("pA_up_phy xi:", pA_up_phy$layers[[3]]$data$xi, "\n")
cat("pA_dn_cog xi:", pA_dn_cog$layers[[3]]$data$xi, "\n")
cat("pA_dn_phy xi:", pA_dn_phy$layers[[3]]$data$xi, "\n")

# =============================================================================
# Panel B — sin baseline
# =============================================================================

pB_up_cog <- ggplot(pd[panel_short == "Panel B" & coef == "Theta_up_Cog"],
                    aes(x = estimate)) +
  geom_histogram(bins = 55, fill = COL_B, alpha = .78,
                 color = "white", linewidth = .12) +
  geom_vline(xintercept = 0, color = "gray55",
             linewidth = .4, linetype = "dotted") +
  ggtitle(coef_labels[["Theta_up_Cog"]]) + th

pB_up_phy <- ggplot(pd[panel_short == "Panel B" & coef == "Theta_up_Phy"],
                    aes(x = estimate)) +
  geom_histogram(bins = 55, fill = COL_B, alpha = .78,
                 color = "white", linewidth = .12) +
  geom_vline(xintercept = 0, color = "gray55",
             linewidth = .4, linetype = "dotted") +
  ggtitle(coef_labels[["Theta_up_Phy"]]) + th

pB_dn_cog <- ggplot(pd[panel_short == "Panel B" & coef == "Theta_dn_Cog"],
                    aes(x = estimate)) +
  geom_histogram(bins = 55, fill = COL_B, alpha = .78,
                 color = "white", linewidth = .12) +
  geom_vline(xintercept = 0, color = "gray55",
             linewidth = .4, linetype = "dotted") +
  ggtitle(coef_labels[["Theta_dn_Cog"]]) + th

pB_dn_phy <- ggplot(pd[panel_short == "Panel B" & coef == "Theta_dn_Phy"],
                    aes(x = estimate)) +
  geom_histogram(bins = 55, fill = COL_B, alpha = .78,
                 color = "white", linewidth = .12) +
  geom_vline(xintercept = 0, color = "gray55",
             linewidth = .4, linetype = "dotted") +
  ggtitle(coef_labels[["Theta_dn_Phy"]]) + th

# =============================================================================
# Ensamblar con patchwork
# =============================================================================

lbl_A <- ggplot() +
  annotate("text", x = .5, y = .5, label = "Panel A",
           angle = 90, fontface = "bold", size = 3.5) +
  theme_void()

lbl_B <- ggplot() +
  annotate("text", x = .5, y = .5, label = "Panel B",
           angle = 90, fontface = "bold", size = 3.5) +
  theme_void()

row_A <- lbl_A | pA_up_cog | pA_up_phy | pA_dn_cog | pA_dn_phy
row_B <- lbl_B | pB_up_cog | pB_up_phy | pB_dn_cog | pB_dn_phy

p_final <- (row_A / row_B) +
  plot_layout(widths = c(.04, 1, 1, 1, 1)) +
  plot_annotation(
    title    = "Fig. S[X]. Domain-label permutation test",
    subtitle = paste0(
      "Null distributions vs. baseline estimates (vertical lines, Panel A only). ",
      "ATC gap collapses 99.9% under permutation ",
      "(p < 0.001; Panel A: gap = 1.155 \u2192 0.001)."),
    caption  = paste0(
      "Vertical black lines = Panel A baseline estimates. ",
      "Dotted grey lines = zero reference. ",
      "x-axis: permuted estimate (cloglog). y-axis: count."),
    theme = theme(
      plot.title    = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 8, color = "gray35",
                                   margin = margin(b = 4)),
      plot.caption  = element_text(size = 7.5, color = "gray45", hjust = 0)))

out_path <- file.path(output_dir, "figs", "fig_SI_test_ii.png")
ggsave(out_path, p_final, width = 13, height = 7, dpi = 300)
message("Guardado: ", normalizePath(out_path))
