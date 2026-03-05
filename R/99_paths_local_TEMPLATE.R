# ==============================================================================
# 99_paths_local_TEMPLATE.R
#
# Plantilla para rutas locales (NO commitear como paths_local.R)
#
# Copia este archivo como:  R/99_paths_local.R  (ignorando por .gitignore)
# y ajusta las rutas en tu máquina.
#
# Uso sugerido en scripts:
#   if (file.exists("R/99_paths_local.R")) source("R/99_paths_local.R")
#   in_data <- PATH_DYADS_V12
# ==============================================================================

# Ruta local a dyads v12 (si no tienes v13)
PATH_DYADS_V12 <- "datos_eventos_v12_FINAL/all_events_final_enriched_REAL.rds"

# Ruta al nestedness file
PATH_NESTEDNESS <- "dt_con_cs_nestedness.rds"
