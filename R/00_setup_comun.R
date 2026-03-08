# ==============================================================================
# 00_setup_comun.R
#
# Shared session setup. Source this at the top of every estimation script
# before running any analysis block.
#
# DATA STRUCTURE:
#   Triads (source_j, target_i, skill_s). One observation = one diffusion
#   opportunity for skill s from occupation j to occupation i.
#   Dependencies: by source, by target, by skill, by dyad (j, i).
#
# SAMPLING STRATEGY:
#   Sampling unit = source occupation j.
#   Retaining ALL triads of sampled sources ensures:
#     - Source FEs are identified on complete information
#     - All 873 target occupations remain well-represented as destinations
#     - Triadic dependency structure is preserved intact
#     - Clusters (source, target, skill) are valid within the sample
#
# OUTPUTS (written to output/output_SI_identity/):
#   rds/sources_sample_seed42.rds   — sampled source occupations (cached)
#
# OBJECTS CREATED IN GLOBAL ENV:
#   dt            — full triadic dataset (data.table, filtered to diffusion obs)
#   dt_sample     — 50% source sample
#   fml_rhs       — canonical model formula
# ==============================================================================

library(data.table)
library(fixest)

source("R/utils.R")

# ------------------------------------------------------------------------------
# Output directories
# ------------------------------------------------------------------------------
output_dir <- file.path("output", "output_SI_identity")
dir.create(file.path(output_dir, "rds"),    showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(output_dir, "tables"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(output_dir, "figs"),   showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------------------------
# Load data
# ------------------------------------------------------------------------------
if (!exists("dt")) {
  local_paths <- file.path("R", "99_paths_local.R")
  if (file.exists(local_paths)) source(local_paths)
  dt_path <- if (exists("PATH_TRIADIC")) PATH_TRIADIC else "dt_con_cs_nestedness.rds"
  if (!file.exists(dt_path)) {
    stop(
      "Data file not found: ", dt_path,
      "\nCopy R/99_paths_local_TEMPLATE.R to R/99_paths_local.R and set PATH_TRIADIC."
    )
  }
  message("Loading triadic data from: ", dt_path)
  dt <- readRDS(dt_path)
}
setDT(dt)

# ------------------------------------------------------------------------------
# Keep only columns needed for estimation
# NOTE: s_wage and t_wage are retained for descriptive scripts (01_)
# ------------------------------------------------------------------------------
needed <- c(
  "diffusion", "wage_gap", "structural_distance",
  "domain", "source", "target", "skill_name",
  "s_wage", "t_wage"
)
needed <- intersect(needed, names(dt))
dt[, setdiff(names(dt), needed) := NULL]
gc(); gc(); gc()

# ------------------------------------------------------------------------------
# Directional wage variables
# ------------------------------------------------------------------------------
dt[, wage_up   := pmax(0,  wage_gap)]
dt[, wage_down := pmin(0,  wage_gap)]
dt[, up_dummy  := fifelse(wage_gap > 0, 1L, 0L)]
dt[, domain    := factor(domain, levels = c("Cognitive", "Physical"))]
gc(); gc(); gc()

# ------------------------------------------------------------------------------
# 50% source-occupation sample (cached for reproducibility)
# ------------------------------------------------------------------------------
SAMPLE_FRAC  <- 0.50
SEED         <- 42L
ckpt_sources <- file.path(output_dir, "rds", "sources_sample_seed42.rds")

if (file.exists(ckpt_sources)) {
  sources_sample <- readRDS(ckpt_sources)
  message("Loaded cached source sample: ", length(sources_sample), " occupations")
} else {
  set.seed(SEED)
  sources_all    <- unique(dt$source)
  sources_sample <- sample(sources_all,
                           size = round(length(sources_all) * SAMPLE_FRAC))
  saveRDS(sources_sample, ckpt_sources)
  message("Generated source sample: ", length(sources_sample),
          " of ", length(sources_all), " (", round(SAMPLE_FRAC * 100), "%)")
}

dt_sample <- dt[source %in% sources_sample]
message(
  "Triads in sample: ", format(nrow(dt_sample), big.mark = ","), "\n",
  "  Sources: ",  uniqueN(dt_sample$source),
  " | Targets: ", uniqueN(dt_sample$target),
  " | Skills: ",  uniqueN(dt_sample$skill_name)
)

# ------------------------------------------------------------------------------
# wage_q_source: wage quintile of the SOURCE occupation
#
# wage_gap = log_wage_target - log_wage_source is a dyad-level attribute.
# We need a source-level attribute. We derive it as mean(wage_gap) by source:
# since log_wage_source is constant for a given source, mean_j(wage_gap_ji)
# equals mean_j(log_wage_i) - log_wage_source. Under a dense skillscape,
# mean_j(log_wage_i) is approximately constant across sources, making this
# a valid proxy for -log_wage_source (Q5 = highest-wage sources).
# ------------------------------------------------------------------------------
source_wage <- dt_sample[, .(mean_wg = mean(wage_gap, na.rm = TRUE)), by = source]
source_wage[, wage_q_source := as.integer(cut(
  mean_wg,
  breaks         = quantile(mean_wg, probs = 0:5 / 5, na.rm = TRUE),
  labels         = 1:5,
  include.lowest = TRUE
))]
dt_sample[source_wage, on = "source", wage_q_source := i.wage_q_source]
rm(source_wage)

stopifnot(
  "wage_q_source must have no NAs"           = !any(is.na(dt_sample$wage_q_source)),
  "Each source must have exactly one quintile" = all(
    dt_sample[, .(n_q = uniqueN(wage_q_source)), by = source]$n_q == 1
  )
)
gc()

message("wage_q_source OK. Quintile distribution:")
print(dt_sample[, .N, keyby = wage_q_source])

# ------------------------------------------------------------------------------
# Canonical model formula (used by all estimation scripts)
# ------------------------------------------------------------------------------
fml_rhs <- diffusion ~
  (up_dummy + wage_up + wage_down + structural_distance) : domain

message("Setup complete. RAM usage:"); print(gc())