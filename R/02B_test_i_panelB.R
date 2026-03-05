# =============================================================================
# 02B_test_i_panelB.R
# Test (i): Threshold placebo — Panel B (FE: target + skill_name)
# Misma lógica que 02A. SESIÓN LIMPIA. Cerrar R al terminar.
# REQUIERE: baseline_coefs_B.rds y test_i_panelA.rds
# =============================================================================

# [pegar 00_setup_comun.R aquí]

base_B <- readRDS(file.path(output_dir, "rds", "baseline_coefs_B.rds"))

cutoff_grid <- c(-1, -0.5, -0.25, 0, 0.25, 0.5, 1)
results     <- list()

for (c_val in cutoff_grid) {
  message("Test i Panel B | c = ", c_val)

  dt_sample[, wage_up   := pmax(0,  wage_gap - c_val)]
  dt_sample[, wage_down := pmin(0,  wage_gap - c_val)]
  dt_sample[, up_dummy  := as.integer(wage_gap > c_val)]

  m <- feglm(fml_rhs,
             data      = dt_sample,
             family    = binomial("cloglog"),
             fixef     = c("target", "skill_name"),
             cluster   = c("source", "target", "skill_name"),
             lean = TRUE, mem.clean = TRUE, nthreads = 0)

  res <- extract_coefs(m, "Panel B")
  res[, c_shift := c_val]
  results[[as.character(c_val)]] <- res
  rm(m); gc(); gc(); gc()

  dt_sample[, wage_up   := pmax(0,  wage_gap)]
  dt_sample[, wage_down := pmin(0,  wage_gap)]
  dt_sample[, up_dummy  := fifelse(wage_gap > 0, 1L, 0L)]
}

thr_B <- rbindlist(results)

# Validación c=0 vs baseline
thr_c0  <- thr_B[c_shift == 0]
chk     <- merge(thr_c0, base_B, by = c("panel_short", "coef"),
                 suffixes = c("_ti", "_base"))
max_dev <- chk[!is.na(estimate_base) & estimate_base != 0,
               max(abs(estimate_ti - estimate_base) / abs(estimate_base))]
message("Desviación máxima c=0 vs baseline: ", round(max_dev * 100, 4), "%")
if (max_dev > 0.001) {
  warning("ADVERTENCIA: c=0 difiere del baseline en >0.1%.")
} else {
  message("OK — c=0 reproduce el baseline.")
}

# Consolidar Test i completo
thr_out <- rbind(
  readRDS(file.path(output_dir, "rds", "test_i_panelA.rds")),
  thr_B
)
saveRDS(thr_out, file.path(output_dir, "rds", "test_i_threshold.rds"))
fwrite(thr_out,  file.path(output_dir, "tables", "table_SI_test_i.csv"))
message("Test i completo (", nrow(thr_out), " filas). CERRAR R.")

#file.remove(file.path("output_SI_identity", "rds", "test_ii_panelA_ckpt.rds"))
