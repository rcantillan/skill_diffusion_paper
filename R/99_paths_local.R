# ==============================================================================
# 99_paths_local_TEMPLATE.R
#
# Template for machine-specific path configuration.
#
# INSTRUCTIONS:
#   1. Copy this file to R/99_paths_local.R  (already in .gitignore — safe)
#   2. Edit the paths below to match your local setup
#   3. Never commit R/99_paths_local.R to the repository
#
# All estimation scripts source this file at startup:
#   if (file.exists("R/99_paths_local.R")) source("R/99_paths_local.R")
#
# Paths can be absolute or relative to the repository root.
# Relative paths are recommended when data live inside the repo tree.
# ==============================================================================

# ------------------------------------------------------------------------------
# Main triadic dataset
# Used by: R/00_setup_comun.R (and all scripts that source it)
# Fallback if not set: "dt_con_cs_nestedness.rds" (repo root)
# ------------------------------------------------------------------------------
PATH_TRIADIC <- "dt_con_cs_nestedness.rds"

# ------------------------------------------------------------------------------
# Dyad construction outputs
# Used by: R/00_build_dyads.R, R/00_build_events_v13_multithr.R
# ------------------------------------------------------------------------------
PATH_DYADS_V12  <- "datos_eventos_v12_FINAL/all_events_final_enriched_REAL.rds"
PATH_DYADS_V13  <- "datos_eventos_v13_MULTITHR/all_events_v13_multithr.rds"

# ------------------------------------------------------------------------------
# Raw O*NET source files
# Used by: R/00_build_dyads.R, R/00_build_events_v13_multithr.R
# ------------------------------------------------------------------------------
PATH_ONET_2015       <- "data/db_15_1"
PATH_ONET_2024       <- "data/db_29_2_text"
PATH_CROSSWALK       <- "data/crosswalk"
PATH_BLS_WAGES_2015  <- "data/national_M2015_dl.xlsx"