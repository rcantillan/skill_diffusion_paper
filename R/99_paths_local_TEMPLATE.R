# ==============================================================================
# 99_paths_local_TEMPLATE.R
#
# Template for local path configuration (DO NOT commit as 99_paths_local.R)
#
# Copy this file as:  R/99_paths_local.R  (ignored by .gitignore)
# and adjust paths for your machine.
#
# Usage in scripts:
#   if (file.exists("R/99_paths_local.R")) source("R/99_paths_local.R")
# ==============================================================================

# Path to the triadic dataset (used by 00_setup_comun.R)
# 00_setup_comun.R looks for PATH_TRIADIC; falls back to "dt_con_cs_nestedness.rds"
PATH_TRIADIC <- "dt_con_cs_nestedness.rds"

# Path to dyads v12 (used by 00_build_dyads.R)
PATH_DYADS_V12 <- "datos_eventos_v12_FINAL/all_events_final_enriched_REAL.rds"

# Path to nestedness file (if stored separately)
PATH_NESTEDNESS <- "dt_con_cs_nestedness.rds"