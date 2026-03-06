# =============================================================================
# 02A_test_i_panelA.R
# Test (i): Threshold placebo — Panel A
# 7 modelos (uno por valor de c). SESIÓN LIMPIA. Cerrar R al terminar.
#
# QUÉ TESTEA:
#   El modelo define wage_up = max(0, wage_gap) con cutoff en c=0.
#   Si este cutoff es el correcto, los coeficientes Theta_up y Theta_dn
#   deben ser máximos en c=0 y decaer al desplazar el cutoff.
#   Desplazamos c ∈ {-1, -0.5, -0.25, 0, 0.25, 0.5, 1} (en unidades de
#   log-salario). c>0 exige saltos más grandes para llamarlos "arriba";
#   c<0 llama "arriba" incluso a pequeñas diferencias negativas.
#
# ESTRUCTURA TRIÁDICA:
#   wage_gap es propiedad de la díada (j,i) — igual para todos los skills
#   s de esa díada. Desplazar c afecta uniformemente todas las tríadas
#   con esa díada. La estructura triádica no se distorsiona.
#
# VALIDACIÓN INTERNA:
#   c=0 debe reproducir exactamente el baseline. Si no, hay un error
#   en la construcción de dt_sample (se verifica con tolerancia 0.1%).
#
# REQUIERE: baseline_coefs_A.rds (correr 01A primero)
# =============================================================================

# [pegar 00_setup_comun.R aquí]

# Cargar baseline Panel A para verificación
base_A <- readRDS(file.path(output_dir, "rds", "baseline_coefs_A.rds"))

cutoff_grid <- c(-1, -0.5, -0.25, 0, 0.25, 0.5, 1)
results     <- list()

for (c_val in cutoff_grid) {
  message("Test i Panel A | c = ", c_val)

  # Redefinir variables con cutoff desplazado
  dt_sample[, wage_up   := pmax(0,  wage_gap - c_val)]
  dt_sample[, wage_down := pmin(0,  wage_gap - c_val)]
  dt_sample[, up_dummy  := as.integer(wage_gap > c_val)]

  m <- feglm(fml_rhs,
             data      = dt_sample,
             family    = binomial("cloglog"),
             fixef     = c("source", "skill_name"),
             cluster   = c("source", "target", "skill_name"),
             lean = TRUE, mem.clean = TRUE, nthreads = 0)

  res <- extract_coefs(m, "Panel A")
  res[, c_shift := c_val]
  results[[as.character(c_val)]] <- res
  rm(m); gc(); gc(); gc()

  # Restaurar valores originales
  dt_sample[, wage_up   := pmax(0,  wage_gap)]
  dt_sample[, wage_down := pmin(0,  wage_gap)]
  dt_sample[, up_dummy  := fifelse(wage_gap > 0, 1L, 0L)]
}

thr_A <- rbindlist(results)

# ── Validación: c=0 debe reproducir el baseline ───────────────────────────────
thr_c0  <- thr_A[c_shift == 0]
chk     <- merge(thr_c0, base_A, by = c("panel_short", "coef"),
                 suffixes = c("_ti", "_base"))
max_dev <- chk[!is.na(estimate_base) & estimate_base != 0,
               max(abs(estimate_ti - estimate_base) / abs(estimate_base))]
message("Desviación máxima c=0 vs baseline: ", round(max_dev * 100, 4), "%")
if (max_dev > 0.001) {
  warning("ADVERTENCIA: c=0 difiere del baseline en >0.1%. Verificar dt_sample.")
} else {
  message("OK — c=0 reproduce el baseline (tolerancia < 0.1%)")
}

saveRDS(thr_A, file.path(output_dir, "rds", "test_i_panelA.rds"))
message("Test i Panel A completo. CERRAR R.")
