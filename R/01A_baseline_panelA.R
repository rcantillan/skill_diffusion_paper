# =============================================================================
# 01A_baseline_panelA.R
# Baseline — Panel A (FE: source + skill_name)
# Un solo modelo. SESIÓN LIMPIA. Cerrar R al terminar.
#
# Panel A identifica efectos desde el LADO DE ORIGEN:
# los FEs de source absorben todo lo fijo de la ocupación fuente j,
# incluyendo su nivel salarial, tamaño, etc. La variación que identifica
# Theta_up y Theta_dn es within-source: para una misma source j, ¿es más
# probable que adopte skills cognitivos cuando el target está más arriba?
# =============================================================================

# [pegar 00_setup_comun.R aquí]

message("Estimando baseline Panel A (FE: source + skill_name)...")
message("RAM antes del modelo:"); print(gc())

m <- feglm(fml_rhs,
           data      = dt_sample,
           family    = binomial("cloglog"),
           fixef     = c("source", "skill_name"),
           cluster   = c("source", "target", "skill_name"),
           lean = TRUE, mem.clean = TRUE, nthreads = 0)

coefs_A <- extract_coefs(m, "Panel A")
rm(m); gc(); gc(); gc()

saveRDS(coefs_A, file.path(output_dir, "rds", "baseline_coefs_A.rds"))
fwrite(coefs_A,  file.path(output_dir, "tables", "baseline_coefs_A.csv"))

print(coefs_A)
message("Panel A guardado. CERRAR R.")
