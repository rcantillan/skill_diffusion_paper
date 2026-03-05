# ==============================================================================
# 07c_rca_threshold_090_110.R
#
# Umbrales RCA 0.90 y 1.10 — complementan el test (iv) del SI.
# Rango final reportado: 0.90 | 1.00 | 1.10 | 1.25
#
# NOTA: diffusion_090 y diffusion_110 se calculan on-the-fly desde rca_t1_target
# (disponible en el .rds de v13).
# ==============================================================================

source("R/utils.R")

# ==============================================================================
# BLOQUE_090A — Umbral 0.90, Panel A (source + skill FE)
# ==============================================================================
{
  library(data.table); library(fixest)

  OUTPUT_DIR <- "output_rca_threshold"
  DATA_PATH  <- "datos_eventos_v13_MULTITHR/all_events_v13_multithr.rds"
  SAVE_FILE  <- file.path(OUTPUT_DIR, "rds", "rca_thr090_A.rds")

  dir.create(file.path(OUTPUT_DIR, "rds"), showWarnings = FALSE, recursive = TRUE)

  dt <- readRDS(DATA_PATH); setDT(dt)
  dt <- dt[rca_t0_target < 0.90]
  dt[, diffusion := fifelse(rca_t1_target >= 0.90, 1L, 0L)]

  needed <- c("diffusion", "wage_gap", "structural_distance",
              "domain", "source", "target", "skill_name")
  dt[, setdiff(names(dt), needed) := NULL]
  gc(); gc()

  dt[, wage_up   := pmax(0,  wage_gap)]
  dt[, wage_down := pmin(0,  wage_gap)]
  dt[, up_dummy  := fifelse(wage_gap > 0, 1L, 0L)]
  dt[, domain    := factor(domain, levels = c("Cognitive", "Physical"))]
  gc(); gc()

  message("=== Panel A | threshold = 0.90 ===")
  message("N filas:       ", format(nrow(dt), big.mark = ","))
  message("Tasa adopción: ", round(mean(dt$diffusion, na.rm = TRUE), 4))
  stopifnot("dt vacío" = nrow(dt) > 0,
            "tasa > 0" = mean(dt$diffusion, na.rm = TRUE) > 0,
            "tasa < 1" = mean(dt$diffusion, na.rm = TRUE) < 1)


  m <- feglm(
    diffusion ~ (up_dummy + wage_up + wage_down + structural_distance) : domain,
    data    = dt,
    family  = binomial("cloglog"),
    fixef   = c("source", "skill_name"),
    cluster = c("source", "target", "skill_name"),
    lean = TRUE, mem.clean = TRUE, nthreads = 0
  )

  res <- extract_coefs(m, "Panel A", 0.90)
  saveRDS(res, SAVE_FILE); print(res)
  message("Guardado: ", SAVE_FILE, " — CERRAR R.")
}

# ==============================================================================
# BLOQUE_090B — Umbral 0.90, Panel B (target + skill FE)
# ==============================================================================
{
  library(data.table); library(fixest)

  OUTPUT_DIR <- "output_rca_threshold"
  DATA_PATH  <- "datos_eventos_v13_MULTITHR/all_events_v13_multithr.rds"
  SAVE_FILE  <- file.path(OUTPUT_DIR, "rds", "rca_thr090_B.rds")

  dir.create(file.path(OUTPUT_DIR, "rds"), showWarnings = FALSE, recursive = TRUE)

  dt <- readRDS(DATA_PATH); setDT(dt)
  dt <- dt[rca_t0_target < 0.90]
  dt[, diffusion := fifelse(rca_t1_target >= 0.90, 1L, 0L)]

  needed <- c("diffusion", "wage_gap", "structural_distance",
              "domain", "source", "target", "skill_name")
  dt[, setdiff(names(dt), needed) := NULL]
  gc(); gc()

  dt[, wage_up   := pmax(0,  wage_gap)]
  dt[, wage_down := pmin(0,  wage_gap)]
  dt[, up_dummy  := fifelse(wage_gap > 0, 1L, 0L)]
  dt[, domain    := factor(domain, levels = c("Cognitive", "Physical"))]
  gc(); gc()

  message("=== Panel B | threshold = 0.90 ===")
  message("N filas:       ", format(nrow(dt), big.mark = ","))
  message("Tasa adopción: ", round(mean(dt$diffusion, na.rm = TRUE), 4))
  stopifnot("dt vacío" = nrow(dt) > 0,
            "tasa > 0" = mean(dt$diffusion, na.rm = TRUE) > 0,
            "tasa < 1" = mean(dt$diffusion, na.rm = TRUE) < 1)


  m <- feglm(
    diffusion ~ (up_dummy + wage_up + wage_down + structural_distance) : domain,
    data    = dt,
    family  = binomial("cloglog"),
    fixef   = c("target", "skill_name"),
    cluster = c("source", "target", "skill_name"),
    lean = TRUE, mem.clean = TRUE, nthreads = 0
  )

  res <- extract_coefs(m, "Panel B", 0.90)
  saveRDS(res, SAVE_FILE); print(res)
  message("Guardado: ", SAVE_FILE, " — CERRAR R.")
}

# ==============================================================================
# BLOQUE_110A — Umbral 1.10, Panel A
# ==============================================================================
{
  library(data.table); library(fixest)

  OUTPUT_DIR <- "output_rca_threshold"
  DATA_PATH  <- "datos_eventos_v13_MULTITHR/all_events_v13_multithr.rds"
  SAVE_FILE  <- file.path(OUTPUT_DIR, "rds", "rca_thr110_A.rds")

  dir.create(file.path(OUTPUT_DIR, "rds"), showWarnings = FALSE, recursive = TRUE)

  dt <- readRDS(DATA_PATH); setDT(dt)
  dt[, diffusion := fifelse(rca_t1_target >= 1.10, 1L, 0L)]

  needed <- c("diffusion", "wage_gap", "structural_distance",
              "domain", "source", "target", "skill_name")
  dt[, setdiff(names(dt), needed) := NULL]
  gc(); gc()

  dt[, wage_up   := pmax(0,  wage_gap)]
  dt[, wage_down := pmin(0,  wage_gap)]
  dt[, up_dummy  := fifelse(wage_gap > 0, 1L, 0L)]
  dt[, domain    := factor(domain, levels = c("Cognitive", "Physical"))]
  gc(); gc()

  message("=== Panel A | threshold = 1.10 ===")
  message("N filas:       ", format(nrow(dt), big.mark = ","))
  message("Tasa adopción: ", round(mean(dt$diffusion, na.rm = TRUE), 4))
  stopifnot("dt vacío" = nrow(dt) > 0,
            "tasa > 0" = mean(dt$diffusion, na.rm = TRUE) > 0,
            "tasa < 1" = mean(dt$diffusion, na.rm = TRUE) < 1)


  m <- feglm(
    diffusion ~ (up_dummy + wage_up + wage_down + structural_distance) : domain,
    data    = dt,
    family  = binomial("cloglog"),
    fixef   = c("source", "skill_name"),
    cluster = c("source", "target", "skill_name"),
    lean = TRUE, mem.clean = TRUE, nthreads = 0
  )

  res <- extract_coefs(m, "Panel A", 1.10)
  saveRDS(res, SAVE_FILE); print(res)
  message("Guardado: ", SAVE_FILE, " — CERRAR R.")
}

# ==============================================================================
# BLOQUE_110B — Umbral 1.10, Panel B
# ==============================================================================
{
  library(data.table); library(fixest)

  OUTPUT_DIR <- "output_rca_threshold"
  DATA_PATH  <- "datos_eventos_v13_MULTITHR/all_events_v13_multithr.rds"
  SAVE_FILE  <- file.path(OUTPUT_DIR, "rds", "rca_thr110_B.rds")

  dir.create(file.path(OUTPUT_DIR, "rds"), showWarnings = FALSE, recursive = TRUE)

  dt <- readRDS(DATA_PATH); setDT(dt)
  dt[, diffusion := fifelse(rca_t1_target >= 1.10, 1L, 0L)]

  needed <- c("diffusion", "wage_gap", "structural_distance",
              "domain", "source", "target", "skill_name")
  dt[, setdiff(names(dt), needed) := NULL]
  gc(); gc()

  dt[, wage_up   := pmax(0,  wage_gap)]
  dt[, wage_down := pmin(0,  wage_gap)]
  dt[, up_dummy  := fifelse(wage_gap > 0, 1L, 0L)]
  dt[, domain    := factor(domain, levels = c("Cognitive", "Physical"))]
  gc(); gc()

  message("=== Panel B | threshold = 1.10 ===")
  message("N filas:       ", format(nrow(dt), big.mark = ","))
  message("Tasa adopción: ", round(mean(dt$diffusion, na.rm = TRUE), 4))
  stopifnot("dt vacío" = nrow(dt) > 0,
            "tasa > 0" = mean(dt$diffusion, na.rm = TRUE) > 0,
            "tasa < 1" = mean(dt$diffusion, na.rm = TRUE) < 1)


  m <- feglm(
    diffusion ~ (up_dummy + wage_up + wage_down + structural_distance) : domain,
    data    = dt,
    family  = binomial("cloglog"),
    fixef   = c("target", "skill_name"),
    cluster = c("source", "target", "skill_name"),
    lean = TRUE, mem.clean = TRUE, nthreads = 0
  )

  res <- extract_coefs(m, "Panel B", 1.10)
  saveRDS(res, SAVE_FILE); print(res)
  message("Guardado: ", SAVE_FILE, " — CERRAR R.")
}

# ==============================================================================
# BLOQUE_CONSOLIDAR_FINAL — combina: 0.90 | 1.00 | 1.10 | 1.25
# ==============================================================================
{
  library(data.table)
  library(ggplot2)

  OUTPUT_DIR <- "output_rca_threshold"
  ATC_COEFS  <- c("Theta_up_Cog", "Theta_dn_Cog", "Theta_up_Phy", "Theta_dn_Phy")

  files_final <- list(
    "rca_thr090_A.rds",
    "rca_thr090_B.rds",
    "rca_thr100_A.rds",
    "rca_thr100_B.rds",
    "rca_thr110_A.rds",
    "rca_thr110_B.rds",
    "rca_thr125_A.rds",
    "rca_thr125_B.rds"
  )

  all_thr <- rbindlist(
    lapply(files_final, function(f) readRDS(file.path(OUTPUT_DIR, "rds", f))),
    fill = TRUE
  )

  thr_wide <- dcast(
    all_thr[coef %in% ATC_COEFS],
    coef + panel_short ~ threshold,
    value.var = "estimate"
  )

  old_names <- intersect(c("0.9","1","1.1","1.25"), names(thr_wide))
  new_names <- c("thr_090","thr_100","thr_110","thr_125")[
    match(old_names, c("0.9","1","1.1","1.25"))]
  setnames(thr_wide, old_names, new_names)

  dev_cols <- intersect(c("thr_090","thr_110","thr_125"), names(thr_wide))
  thr_wide[, max_pct_dev := apply(
    abs(.SD - thr_100) / abs(thr_100) * 100,
    1, max, na.rm = TRUE
  ), .SDcols = dev_cols]

  fwrite(thr_wide,
         file.path(OUTPUT_DIR, "tables", "table_SI_rca_threshold_FINAL.csv"))

  measure_cols <- intersect(c("thr_090","thr_100","thr_110","thr_125"), names(thr_wide))
  thr_long <- melt(thr_wide,
                   id.vars      = c("coef","panel_short","max_pct_dev"),
                   measure.vars = measure_cols,
                   variable.name = "thr_label",
                   value.name    = "estimate")

  thr_long[, thr_label := fcase(
    thr_label == "thr_090", "RCA\u22650.90",
    thr_label == "thr_100", "RCA\u22651.00",
    thr_label == "thr_110", "RCA\u22651.10",
    thr_label == "thr_125", "RCA\u22651.25"
  )]
  thr_long[, thr_label := factor(thr_label,
    levels = c("RCA\u22650.90","RCA\u22651.00",
               "RCA\u22651.10","RCA\u22651.25"))]

  coef_labels <- c(
    Theta_up_Cog = "\u0398\u2191 Cognitive",
    Theta_dn_Cog = "\u0398\u2193 Cognitive",
    Theta_up_Phy = "\u0398\u2191 Physical",
    Theta_dn_Phy = "\u0398\u2193 Physical"
  )
  thr_long[, coef_label := factor(
    coef_labels[coef],
    levels = c("\u0398\u2191 Cognitive","\u0398\u2191 Physical",
               "\u0398\u2193 Cognitive","\u0398\u2193 Physical")
  )]

  se_dt <- all_thr[coef %in% ATC_COEFS,
                   .(panel_short, coef, threshold, se)]
  se_dt[, thr_label := fcase(
    threshold == 0.90, "RCA\u22650.90",
    threshold == 1.00, "RCA\u22651.00",
    threshold == 1.10, "RCA\u22651.10",
    threshold == 1.25, "RCA\u22651.25"
  )]
  se_dt[, thr_label := factor(thr_label,
    levels = c("RCA\u22650.90","RCA\u22651.00",
               "RCA\u22651.10","RCA\u22651.25"))]
  se_dt[, coef_label := factor(coef_labels[coef],
    levels = c("\u0398\u2191 Cognitive","\u0398\u2191 Physical",
               "\u0398\u2193 Cognitive","\u0398\u2193 Physical"))]

  thr_long <- merge(thr_long, se_dt[, .(panel_short, coef_label, thr_label, se)],
                    by = c("panel_short","coef_label","thr_label"), all.x = TRUE)
  thr_long[, ymin := estimate - 1.96 * se]
  thr_long[, ymax := estimate + 1.96 * se]

  p_thr <- ggplot(thr_long,
    aes(x = thr_label, y = estimate,
        color = panel_short, group = panel_short,
        fill  = panel_short)) +
    geom_hline(yintercept = 0, color = "gray70", linewidth = .3) +
    geom_vline(xintercept = 2, linetype = "dotted",
               color = "gray50", linewidth = .4) +
    geom_ribbon(aes(ymin = ymin, ymax = ymax),
                alpha = 0.12, color = NA) +
    geom_line(linewidth = .7) +
    geom_point(size = 2.8) +
    facet_wrap(~ coef_label, scales = "free_y", ncol = 2) +
    scale_color_manual(values = c("Panel A" = "#1f6b8e",
                                  "Panel B" = "#c0392b")) +
    scale_fill_manual(values  = c("Panel A" = "#1f6b8e",
                                  "Panel B" = "#c0392b")) +
    labs(
      title    = "Fig. S1. RCA threshold sensitivity",
      subtitle = paste0(
        "ATC parameters re-estimated across four specialization thresholds ",
        "(RCA \u2265 0.90 to 1.25).\n",
        "Ribbons: 95% CI. Dotted vertical line: baseline (RCA \u2265 1.00)."),
      x     = "RCA threshold",
      y     = "Estimate (complementary log-log scale)",
      color = NULL, fill = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom",
          strip.text      = element_text(face = "bold"),
          axis.text.x     = element_text(size = 9))

  ggsave(file.path(OUTPUT_DIR, "figs", "fig_SI_rca_threshold_FINAL.png"),
         p_thr, width = 10, height = 8, dpi = 300)

  message("\nTodo guardado en: ", normalizePath(OUTPUT_DIR))
}
