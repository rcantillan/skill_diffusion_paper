# =============================================================================
# 01B_baseline_panelB.R
# Baseline — Panel B (FE: target + skill_name)
# Un solo modelo. SESIÓN LIMPIA. Cerrar R al terminar.
#
# Panel B identifica efectos desde el LADO DE DESTINO:
# los FEs de target absorben todo lo fijo de la ocupación destino i.
# La variación que identifica Theta_up y Theta_dn es within-target:
# para un mismo target i, ¿adopta más skills cognitivos cuando vienen
# de sources con salarios más bajos (mayor wage_gap positivo)?
# La concordancia de signos entre Panel A y Panel B es la evidencia
# central de identificación del paper.
# =============================================================================

# [pegar 00_setup_comun.R aquí]

message("Estimando baseline Panel B (FE: target + skill_name)...")
message("RAM antes del modelo:"); print(gc())

m <- feglm(fml_rhs,
           data      = dt_sample,
           family    = binomial("cloglog"),
           fixef     = c("target", "skill_name"),
           cluster   = c("source", "target", "skill_name"),
           lean = TRUE, mem.clean = TRUE, nthreads = 0)

coefs_B <- extract_coefs(m, "Panel B")
rm(m); gc(); gc(); gc()

saveRDS(coefs_B, file.path(output_dir, "rds", "baseline_coefs_B.rds"))
fwrite(coefs_B,  file.path(output_dir, "tables", "baseline_coefs_B.csv"))

# Consolidar baseline completo
base_coefs <- rbind(
  readRDS(file.path(output_dir, "rds", "baseline_coefs_A.rds")),
  coefs_B
)
saveRDS(base_coefs, file.path(output_dir, "rds", "baseline_coefs.rds"))
fwrite(base_coefs,  file.path(output_dir, "tables", "baseline_coefs_full.csv"))

# Concordancia de signos entre paneles (verificación inmediata)
ATC_COEFS <- c("Theta_up_Cog", "Theta_dn_Cog", "Theta_up_Phy", "Theta_dn_Phy")
conc_check <- dcast(base_coefs[coef %in% ATC_COEFS],
                    coef ~ panel_short, value.var = "estimate")
conc_check[, agrees_sign := sign(`Panel A`) == sign(`Panel B`)]
message("\nConcordancia de signos Panel A vs Panel B:")
print(conc_check)

print(base_coefs)
message("Baseline completo y guardado. CERRAR R.")
