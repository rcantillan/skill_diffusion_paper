# ============================================================
# S8.2 — Physical_HN como archetype separado
# ============================================================

library(data.table)
library(fixest)
library(broom)
library(dplyr)

# Optional: load dyads into `dt` if not already in memory
if (!exists("dt")) {
  if (file.exists("R/99_paths_local.R")) source("R/99_paths_local.R")
  if (exists("PATH_DYADS_V12")) {
    message("Cargando dyads desde PATH_DYADS_V12...")
    dt <- readRDS(PATH_DYADS_V12)
    setDT(dt)
  } else {
    stop("No existe `dt` en memoria y no se encontró PATH_DYADS_V12. Carga el dyads .rds o define PATH_DYADS_V12 en R/99_paths_local.R")
  }
}


# Nota: este script asume que 'dt' es el dataset de dyads (no el dt de nestedness).
# Si no lo tienes en memoria, ajusta la ruta al .rds de dyads (ej. all_events_final_*.rds)
# y cárgalo aquí.
#
# Ejemplo:
# dt <- readRDS("datos_eventos_v12_FINAL/all_events_final_enriched_REAL.rds")
# setDT(dt)

# Umbral within-domain para Physical y Cognitive
threshold_phy <- dt[domain == "Physical",  median(cs, na.rm = TRUE)]
threshold_cog <- dt[domain == "Cognitive", median(cs, na.rm = TRUE)]

# Nueva tipología con 4 archetypes
dt[, atc_archetype_4 := fcase(
  domain == "Cognitive" & cs >= threshold_cog, "SC_Scaffolding",
  domain == "Cognitive" & cs <  threshold_cog, "SC_Specialized",
  domain == "Physical"  & cs <  threshold_phy, "Physical_Terminal",
  domain == "Physical"  & cs >= threshold_phy, "Physical_HN"
)]

# SC_Specialized como referencia
dt[, atc_archetype_4 := factor(atc_archetype_4,
                                levels = c("SC_Specialized",
                                           "SC_Scaffolding",
                                           "Physical_Terminal",
                                           "Physical_HN"))]

# Verificar distribución antes de estimar
print(dt[, .(n_skills = uniqueN(skill_name)), by = atc_archetype_4])

gc(); gc(); gc()

# ── Panel A: source + skill FE ───────────────────────────────────────────────-
m_s8_panelA <- feglm(
  diffusion ~
    (wage_up + wage_down + up_dummy + structural_distance) : atc_archetype_4,
  data    = sample_frac(dt, .5, seed = 42),
  family  = binomial(link = "cloglog"),
  fixef   = c("source", "skill_name"),
  cluster = c("source", "target", "skill_name"),
  nthreads = 0, mem.clean = TRUE, lean = TRUE
)

gc(); gc(); gc()

# ── Panel B: target + skill FE ─────────────────────────────────────────────--
m_s8_panelB <- feglm(
  diffusion ~
    (wage_up + wage_down + up_dummy + structural_distance) : atc_archetype_4,
  data    = sample_frac(dt, .5, seed = 42),
  family  = binomial(link = "cloglog"),
  fixef   = c("target", "skill_name"),
  cluster = c("source", "target", "skill_name"),
  nthreads = 0, mem.clean = TRUE, lean = TRUE
)

gc(); gc(); gc()

extract_atc <- function(model, panel_label) {
  tidy(model, conf.int = TRUE) %>%
    filter(grepl("wage_up|wage_down", term)) %>%
    filter(grepl("atc_archetype_4", term)) %>%
    mutate(
      panel     = panel_label,
      direction = ifelse(grepl("wage_up", term), "beta_up", "beta_down"),
      archetype = case_when(
        grepl("SC_Scaffolding",    term) ~ "SC_Scaffolding",
        grepl("SC_Specialized",    term) ~ "SC_Specialized",
        grepl("Physical_Terminal", term) ~ "Physical_Terminal",
        grepl("Physical_HN",       term) ~ "Physical_HN"
      )
    ) %>%
    select(panel, archetype, direction, estimate, std.error, conf.low, conf.high, p.value)
}

results_s8 <- bind_rows(
  extract_atc(m_s8_panelA, "Panel A"),
  extract_atc(m_s8_panelB, "Panel B")
)

print(results_s8)

saveRDS(results_s8, "results_s8_physical_hn.rds")
