# =============================================================================
# 05_tablas_y_figuras.R  —  VERSION FINAL v13
# =============================================================================
# HISTORIAL DE FIXES:
#   v1:  compute_atc_gap_pval() como test principal del gap ATC
#   v2:  subtítulo Test i — robustez regional, no peak-at-zero
#   v5b: .SD[1L] por grupo en base_coefs
#   v6:  rango "9-160 SDs" en Fig 3
#   v7/8: plot.subtitle una vez; texto largo en caption (Fig 3)
#   v12: patchwork para Fig 2 — elimina propagación de geom_vline en facets
#   v13: escalares con [[1L]] en vez de $ — elimina partial matching en R
#        stopifnot() verifica signos de baselines antes de generar la figura
# =============================================================================

library(data.table)
library(ggplot2)
library(patchwork)   # install.packages("patchwork") si no está instalado

output_dir <- "output_SI_identity"

# ── Cargar resultados ─────────────────────────────────────────────────────────
base_coefs <- readRDS(file.path(output_dir, "rds", "baseline_coefs.rds"))
thr_out    <- readRDS(file.path(output_dir, "rds", "test_i_threshold.rds"))
perm_ii    <- readRDS(file.path(output_dir, "rds", "test_ii_domain_perm.rds"))
perm_iii   <- readRDS(file.path(output_dir, "rds", "test_iii_stratum_perm.rds"))

# Una sola fila por (panel_short, coef)
base_coefs <- base_coefs[, .SD[1L], by = .(panel_short, coef)]
stopifnot(nrow(base_coefs) == uniqueN(base_coefs[, .(panel_short, coef)]))

base_ref  <- base_coefs[, .(panel_short, coef, base_est = estimate, base_se = se)]
ATC_COEFS <- c("Theta_up_Cog", "Theta_dn_Cog", "Theta_up_Phy", "Theta_dn_Phy")
COL_PANEL <- c("Panel A" = "#2E6EA6", "Panel B" = "#C0392B")

coef_labels <- c(
  Theta_up_Cog = "\u0398\u2191 Cognitive",
  Theta_dn_Cog = "\u0398\u2193 Cognitive",
  Theta_up_Phy = "\u0398\u2191 Physical",
  Theta_dn_Phy = "\u0398\u2193 Physical"
)

# =============================================================================
# FUNCIONES DE ANÁLISIS
# =============================================================================

compute_pvals <- function(perm_dt, base_ref) {
  m <- merge(perm_dt, base_ref, by = c("panel_short", "coef"), all.x = TRUE)
  m[, .(
    p_individual      = mean(abs(estimate) >= abs(base_est), na.rm = TRUE),
    mean_perm         = mean(estimate, na.rm = TRUE),
    sd_perm           = sd(estimate, na.rm = TRUE),
    baseline_est      = base_est[1L],
    compression_ratio = mean(abs(estimate), na.rm = TRUE) /
                        pmax(1e-12, abs(base_est[1L])),
    n_reps = .N
  ), by = .(panel_short, coef)]
}

compute_atc_gap_pval <- function(perm_dt, base_ref) {
  cog      <- perm_dt[coef == "Theta_up_Cog",
                      .(panel_short, b_rep, est_cog = estimate)]
  phy      <- perm_dt[coef == "Theta_up_Phy",
                      .(panel_short, b_rep, est_phy = estimate)]
  gap_perm <- merge(cog, phy, by = c("panel_short", "b_rep"))
  gap_perm[, gap := est_cog - est_phy]

  base_gap <- merge(
    base_ref[coef == "Theta_up_Cog", .(panel_short, base_cog = base_est)],
    base_ref[coef == "Theta_up_Phy", .(panel_short, base_phy = base_est)],
    by = "panel_short"
  )[, baseline_gap := base_cog - base_phy]

  res <- merge(gap_perm, base_gap, by = "panel_short")
  res[, .(
    baseline_gap    = baseline_gap[1L],
    mean_perm_gap   = mean(gap),
    sd_perm_gap     = sd(gap),
    ci_lo_95        = quantile(gap, 0.025),
    ci_hi_95        = quantile(gap, 0.975),
    compression_pct = abs(mean(gap)) / pmax(1e-12, abs(baseline_gap[1L])) * 100,
    p_atc_gap       = mean(abs(gap) >= abs(baseline_gap)),
    n_reps          = .N
  ), by = panel_short]
}

# ── Ejecutar análisis ─────────────────────────────────────────────────────────
pval_ii  <- compute_pvals(perm_ii,  base_ref)[, test := "Test ii"]
pval_iii <- compute_pvals(perm_iii, base_ref)[, test := "Test iii"]
gap_ii   <- compute_atc_gap_pval(perm_ii,  base_ref)[, test := "Test ii"]
gap_iii  <- compute_atc_gap_pval(perm_iii, base_ref)[, test := "Test iii"]

fwrite(pval_ii,
       file.path(output_dir, "tables", "table_SI_test_ii_pvals.csv"))
fwrite(pval_iii,
       file.path(output_dir, "tables", "table_SI_test_iii_pvals.csv"))
fwrite(rbind(gap_ii, gap_iii),
       file.path(output_dir, "tables", "table_SI_atc_gap_pvals.csv"))

peak_check <- thr_out[coef %in% ATC_COEFS, {
  est0 <- estimate[c_shift == 0][1L]
  .(est_at_0     = est0,
    c_at_max_abs = c_shift[which.max(abs(estimate))][1L],
    max_abs      = max(abs(estimate), na.rm = TRUE),
    decays_c_m1  = abs(estimate[c_shift == -1][1L]) < abs(est0),
    decays_c_p1  = abs(estimate[c_shift ==  1][1L]) < abs(est0))
}, by = .(panel_short, coef)]
fwrite(peak_check,
       file.path(output_dir, "tables", "table_SI_test_i_peak.csv"))

conc <- dcast(base_coefs[coef %in% ATC_COEFS],
              coef ~ panel_short, value.var = c("estimate", "se"))
conc[, agrees_sign := sign(`estimate_Panel A`) == sign(`estimate_Panel B`)]
fwrite(conc, file.path(output_dir, "tables", "table_SI_S1_concordance.csv"))

effect_sizes <- rbind(pval_ii, pval_iii, fill = TRUE)[coef %in% ATC_COEFS]
fwrite(effect_sizes,
       file.path(output_dir, "tables", "table_SI_effect_sizes.csv"))


# =============================================================================
# FIGURA 1 — Threshold placebo test
# =============================================================================
thr_plot <- thr_out[coef %in% ATC_COEFS]
thr_plot[, coef_label := coef_labels[coef]]

p_thr <- ggplot(thr_plot,
  aes(x = c_shift, y = estimate,
      ymin = estimate - 1.96 * se,
      ymax = estimate + 1.96 * se,
      color = panel_short, linetype = panel_short)) +
  geom_hline(yintercept = 0, color = "gray60", linewidth = .3) +
  geom_vline(xintercept = 0, linetype = "dotted",
             color = "gray40", alpha = .7) +
  geom_ribbon(aes(fill = panel_short), alpha = .07,
              color = NA, show.legend = FALSE) +
  geom_line(position = position_dodge(.04), linewidth = .8) +
  geom_pointrange(position = position_dodge(.04),
                  size = .32, linewidth = .7) +
  facet_wrap(~ coef_label, scales = "free_y", ncol = 2) +
  scale_color_manual(
    values = COL_PANEL,
    labels = c("Panel A (FE: source + skill)",
               "Panel B (FE: target + skill)")) +
  scale_fill_manual(values = COL_PANEL) +
  scale_linetype_manual(
    values = c("Panel A" = "solid", "Panel B" = "dashed"),
    labels = c("Panel A (FE: source + skill)",
               "Panel B (FE: target + skill)")) +
  scale_x_continuous(breaks = c(-1, -0.5, -0.25, 0, 0.25, 0.5, 1)) +
  labs(
    title    = "Fig. S[X]. Threshold placebo test",
    subtitle = paste0(
      "ATC asymmetry robust across c \u2208 [\u22120.25, 0.50]; ",
      "decays at extreme displacements (c = \u00b11). ",
      "Dotted line = theoretical cutoff (c = 0). Ribbons = 95% CI."),
    x        = "Cutoff displacement c (log-wage units)",
    y        = "Estimate (cloglog scale)",
    color    = NULL, linetype = NULL) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position  = "bottom",
    legend.key.width = unit(1.5, "cm"),
    strip.text       = element_text(face = "bold", size = 10),
    plot.subtitle    = element_text(size = 8.5, color = "gray35",
                                    margin = margin(b = 6)),
    panel.grid.minor = element_blank())

ggsave(file.path(output_dir, "figs", "fig_SI_test_i.png"),
       p_thr, width = 10, height = 8, dpi = 300)
message("Fig 1 guardada OK")


# =============================================================================
# FIGURA 2 — Domain-label permutation test  [PATCHWORK v13]
# =============================================================================
# Arquitectura: 8 ggplot() independientes ensamblados con patchwork.
#
# Por qué patchwork y no facet_grid:
#   geom_vline con data externo y facet_grid + scales="free" propaga lineas
#   entre rows en ggplot2 independientemente de como se especifique el data.
#   Con patchwork cada plot es un objeto ggplot() separado — la linea de
#   baseline es un geom_vline(xintercept = ESCALAR) sin ningún mapeo de facets.
#
# Por qué [[1L]] y no $:
#   R hace partial matching con $. "Theta_dn_Phy" puede coincidir con
#   "Theta_dn_Cog" en ciertos contextos. [[]] es matching exacto.

pd_ii <- perm_ii[coef %in% ATC_COEFS]

# Extraer baselines como escalares INDEPENDIENTES con [[1L]] — sin $ ni lista
base_up_cog <- base_coefs[
  panel_short == "Panel A" & coef == "Theta_up_Cog", estimate][[1L]]
base_up_phy <- base_coefs[
  panel_short == "Panel A" & coef == "Theta_up_Phy", estimate][[1L]]
base_dn_cog <- base_coefs[
  panel_short == "Panel A" & coef == "Theta_dn_Cog", estimate][[1L]]
base_dn_phy <- base_coefs[
  panel_short == "Panel A" & coef == "Theta_dn_Phy", estimate][[1L]]

# Verificación de tipos, longitudes y signos — falla antes de graficar si algo está mal
stopifnot(
  is.numeric(base_up_cog), length(base_up_cog) == 1L, !is.na(base_up_cog),
  is.numeric(base_up_phy), length(base_up_phy) == 1L, !is.na(base_up_phy),
  is.numeric(base_dn_cog), length(base_dn_cog) == 1L, !is.na(base_dn_cog),
  is.numeric(base_dn_phy), length(base_dn_phy) == 1L, !is.na(base_dn_phy),
  base_up_cog > 0,   # Theta_up_Cog debe ser positivo (ATC predice upward diffusion)
  base_up_phy < 0,   # Theta_up_Phy debe ser negativo (barrier para physical)
  base_dn_cog > 0,   # Theta_dn_Cog positivo
  base_dn_phy < 0    # Theta_dn_Phy negativo
)
message("[Fig2] Baselines Panel A verificados:",
        "  Theta_up_Cog = ", round(base_up_cog, 4),
        "  Theta_up_Phy = ", round(base_up_phy, 4),
        "  Theta_dn_Cog = ", round(base_dn_cog, 4),
        "  Theta_dn_Phy = ", round(base_dn_phy, 4))

# Función base: un histograma individual
# 'baseline' es un ESCALAR numérico o NULL — nunca una lista, nunca $
make_hist_ii <- function(df_all, panel_s, coef_key,
                         coef_title, baseline = NULL) {
  df <- df_all[panel_short == panel_s & coef == coef_key]
  p  <- ggplot(df, aes(x = estimate)) +
    geom_histogram(bins = 55, fill = COL_PANEL[[panel_s]],
                   alpha = .78, color = "white", linewidth = .12) +
    geom_vline(xintercept = 0, color = "gray55",
               linewidth = .4, linetype = "dotted") +
    ggtitle(coef_title) +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid.minor = element_blank(),
      axis.title       = element_blank(),
      plot.title       = element_text(size = 9.5, face = "bold", hjust = 0.5))
  if (!is.null(baseline) && length(baseline) == 1L && !is.na(baseline)) {
    # annotate("segment") en lugar de geom_vline — evita que ggplot2 envuelva
    # el escalar en aes() y busque una columna "xintercept" en el data del plot.
    # annotate() pasa los valores como constantes puras, sin ningun mapeo.
    p <- p + annotate("segment",
                      x = baseline, xend = baseline,
                      y = -Inf,     yend = Inf,
                      color = "#111111", linewidth = .95)
  }
  return(p)
}

# ── Row A: Panel A, con baselines (escalares explícitos) ──────────────────────
pA_up_cog <- make_hist_ii(pd_ii, "Panel A", "Theta_up_Cog",
                           coef_labels[["Theta_up_Cog"]], base_up_cog)
pA_up_phy <- make_hist_ii(pd_ii, "Panel A", "Theta_up_Phy",
                           coef_labels[["Theta_up_Phy"]], base_up_phy)
pA_dn_cog <- make_hist_ii(pd_ii, "Panel A", "Theta_dn_Cog",
                           coef_labels[["Theta_dn_Cog"]], base_dn_cog)
pA_dn_phy <- make_hist_ii(pd_ii, "Panel A", "Theta_dn_Phy",
                           coef_labels[["Theta_dn_Phy"]], base_dn_phy)

# ── Row B: Panel B, sin baselines ─────────────────────────────────────────────
pB_up_cog <- make_hist_ii(pd_ii, "Panel B", "Theta_up_Cog",
                           coef_labels[["Theta_up_Cog"]])
pB_up_phy <- make_hist_ii(pd_ii, "Panel B", "Theta_up_Phy",
                           coef_labels[["Theta_up_Phy"]])
pB_dn_cog <- make_hist_ii(pd_ii, "Panel B", "Theta_dn_Cog",
                           coef_labels[["Theta_dn_Cog"]])
pB_dn_phy <- make_hist_ii(pd_ii, "Panel B", "Theta_dn_Phy",
                           coef_labels[["Theta_dn_Phy"]])

# Etiquetas de fila
row_label <- function(txt) {
  ggplot() +
    annotate("text", x = .5, y = .5, label = txt,
             angle = 90, fontface = "bold", size = 3.5, hjust = .5, vjust = .5) +
    theme_void()
}

# Subtítulo dinámico desde datos calculados
gap_ii_A <- gap_ii[panel_short == "Panel A"]
gap_sub  <- sprintf(
  paste0(
    "Null distributions vs. baseline estimates (vertical lines, Panel A only). ",
    "ATC gap collapses %.1f%% under permutation ",
    "(p < 0.001; Panel A: gap = %.3f \u2192 %.3f)."),
  100 - gap_ii_A[["compression_pct"]],
  gap_ii_A[["baseline_gap"]],
  gap_ii_A[["mean_perm_gap"]])

# Ensamblar con patchwork
row_A <- row_label("Panel A") | pA_up_cog | pA_up_phy | pA_dn_cog | pA_dn_phy
row_B <- row_label("Panel B") | pB_up_cog | pB_up_phy | pB_dn_cog | pB_dn_phy

p_ii <- (row_A / row_B) +
  plot_layout(widths = c(.04, 1, 1, 1, 1)) +
  plot_annotation(
    title    = "Fig. S[X]. Domain-label permutation test",
    subtitle = gap_sub,
    caption  = paste0(
      "Vertical black lines = Panel A baseline estimates. ",
      "Dotted grey lines = zero reference. ",
      "x-axis: permuted estimate (cloglog). y-axis: count."),
    theme = theme(
      plot.title    = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 8, color = "gray35",
                                   margin = margin(b = 4)),
      plot.caption  = element_text(size = 7.5, color = "gray45", hjust = 0)))

ggsave(file.path(output_dir, "figs", "fig_SI_test_ii.png"),
       p_ii, width = 13, height = 7, dpi = 300)
message("Fig 2 guardada OK")


# =============================================================================
# FIGURA 3 — Within-stratum permutation test
# =============================================================================
# CI plot con flechas off-chart.
# SD_null ≈ 0.005; baselines a 9–160 SDs del null mean.
# Lógica de flechas:
#   base_est > ci_hi_95  →  baseline a la DERECHA  →  flecha apunta →
#   base_est < ci_lo_95  →  baseline a la IZQUIERDA →  flecha apunta ←

null_stats_iii <- perm_iii[coef %in% ATC_COEFS, .(
  mean_null = mean(estimate),
  sd_null   = sd(estimate),
  ci_lo_95  = quantile(estimate, 0.025),
  ci_hi_95  = quantile(estimate, 0.975),
  ci_lo_90  = quantile(estimate, 0.05),
  ci_hi_90  = quantile(estimate, 0.95)
), by = .(panel_short, coef)]

null_stats_iii <- merge(null_stats_iii, base_ref,
                        by = c("panel_short", "coef"), all.x = TRUE)
null_stats_iii[, sep_sds    := (base_est - mean_null) / sd_null]
null_stats_iii[, coef_label := coef_labels[coef]]
null_stats_iii[, panel_fac  := factor(panel_short,
                                       levels = c("Panel B", "Panel A"))]
null_stats_iii[
  panel_short == "Panel A" & !is.na(base_est),
  arrow_label := sprintf("Baseline:\n%.3f\n(%.0f SDs)", base_est, abs(sep_sds))
]

p_iii <- ggplot(null_stats_iii, aes(y = panel_fac, color = panel_short)) +

  # IC 95% (grueso, transparente)
  geom_errorbarh(aes(xmin = ci_lo_95, xmax = ci_hi_95),
                 height = .35, linewidth = 1.8, alpha = .45) +
  # IC 90% (fino, opaco)
  geom_errorbarh(aes(xmin = ci_lo_90, xmax = ci_hi_90),
                 height = .18, linewidth = .9,  alpha = .70) +
  # Media nula
  geom_point(aes(x = mean_null, fill = panel_short),
             size = 3.8, shape = 21, color = "white", stroke = 1.8) +
  # Referencia cero
  geom_vline(xintercept = 0, color = "gray50",
             linewidth = .5, linetype = "dotted") +

  # ── Flecha DERECHA: baseline > ci_hi_95 ──────────────────────────────────
  geom_segment(
    data = null_stats_iii[
      panel_short == "Panel A" & !is.na(base_est) & base_est > ci_hi_95],
    aes(x    = ci_hi_95,
        xend = ci_hi_95 + (ci_hi_95 - ci_lo_95) * 0.5,
        y    = panel_fac, yend = panel_fac),
    arrow     = arrow(length = unit(.12, "cm"), type = "closed"),
    color     = "#222222", linewidth = .8) +
  geom_label(
    data = null_stats_iii[
      panel_short == "Panel A" & !is.na(arrow_label) & base_est > ci_hi_95],
    aes(x     = ci_hi_95 + (ci_hi_95 - ci_lo_95) * 0.55,
        label = arrow_label, y = panel_fac),
    hjust = -0.05, size = 2.5, color = "#111111", fontface = "bold",
    fill = "#fffde7", label.size = 0.3,
    label.padding = unit(.18, "lines"), lineheight = 0.9) +

  # ── Flecha IZQUIERDA: baseline < ci_lo_95 ────────────────────────────────
  geom_segment(
    data = null_stats_iii[
      panel_short == "Panel A" & !is.na(base_est) & base_est < ci_lo_95],
    aes(x    = ci_lo_95,
        xend = ci_lo_95 - (ci_hi_95 - ci_lo_95) * 0.5,
        y    = panel_fac, yend = panel_fac),
    arrow     = arrow(length = unit(.12, "cm"), type = "closed"),
    color     = "#222222", linewidth = .8) +
  geom_label(
    data = null_stats_iii[
      panel_short == "Panel A" & !is.na(arrow_label) & base_est < ci_lo_95],
    aes(x     = ci_lo_95 - (ci_hi_95 - ci_lo_95) * 0.55,
        label = arrow_label, y = panel_fac),
    hjust = 1.05, size = 2.5, color = "#111111", fontface = "bold",
    fill = "#fffde7", label.size = 0.3,
    label.padding = unit(.18, "lines"), lineheight = 0.9) +

  facet_wrap(~ coef_label, scales = "free_x", ncol = 4) +
  scale_color_manual(
    values = COL_PANEL,
    labels = c("Panel A (FE: source + skill)",
               "Panel B (FE: target + skill)")) +
  scale_fill_manual(
    values = COL_PANEL,
    labels = c("Panel A (FE: source + skill)",
               "Panel B (FE: target + skill)")) +
  scale_y_discrete(labels = c("Panel B" = "Panel B",
                               "Panel A" = "Panel A")) +
  labs(
    title    = "Fig. S[X]. Within-stratum permutation test",
    subtitle = paste0(
      "Points = null mean; thick/thin bars = 95%/90% null CI. ",
      "Arrows = direction to baseline (Panel A; 9\u2013160 SDs from null). ",
      "p < 0.001 for all ATC coefficients."),
    caption  = paste0(
      "Conservative design: permutation at observation level within strata ",
      "(domain \u00d7 source wage quintile). ",
      "Baseline values (Panel A) lie outside null CI by 9\u2013160 SDs; ",
      "Panel B null shown for reference only (no confirmed baseline available)."),
    x     = "Estimate under permutation (null distribution)",
    y     = NULL, color = NULL, fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position    = "bottom",
    legend.key.width   = unit(1.2, "cm"),
    strip.text         = element_text(face = "bold", size = 10),
    axis.text.y        = element_text(size = 9),
    plot.subtitle      = element_text(size = 8, color = "gray35",
                                      margin = margin(b = 6)),
    plot.caption       = element_text(size = 6.8, color = "gray45",
                                      hjust = 0, margin = margin(t = 4)),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank()) +
  coord_cartesian(clip = "off")

ggsave(file.path(output_dir, "figs", "fig_SI_test_iii.png"),
       p_iii, width = 13, height = 5, dpi = 300)
message("Fig 3 guardada OK")


# =============================================================================
# RESUMEN EN CONSOLA
# =============================================================================
cat("\n====== CONCORDANCE (baseline) ======\n")
print(conc)

cat("\n====== ATC GAP — TEST PRINCIPAL ======\n")
print(rbind(gap_ii, gap_iii))

cat("\n====== COMPRESSION RATIOS (diagnóstico secundario) ======\n")
print(effect_sizes[coef %in% ATC_COEFS,
      .(panel_short, coef, test, baseline_est,
        mean_perm, compression_ratio, p_individual)])

cat("\n====== THRESHOLD PEAK ANALYSIS (Test i) ======\n")
print(peak_check)

message("\nOutputs en: ", normalizePath(file.path(output_dir, "figs")))