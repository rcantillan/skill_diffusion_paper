# =============================================================================
# 00_setup_comun.R
# Pegar al inicio de CADA sesión antes de correr cualquier bloque.
#
# ESTRUCTURA DE LOS DATOS:
#   Tríadas (source_j, target_i, skill_s). Una observación = una oportunidad
#   de difusión del skill s desde la ocupación j hacia la ocupación i.
#   Dependencias: por source, por target, por skill, por díada (j,i).
#
# ESTRATEGIA DE MUESTREO:
#   Unidad de muestreo = source occupation j.
#   Al retener TODAS las tríadas de las sources seleccionadas:
#   - Los FEs de source están identificados con información completa
#   - Los targets (873 ocupaciones) siguen bien representados como destinos
#   - La estructura de dependencia triádica se preserva intacta
#   - Los clusters (source, target, skill) son válidos dentro de la muestra
# =============================================================================

library(data.table)
library(fixest)

output_dir <- "output_SI_identity"
dir.create(file.path(output_dir, "rds"),    showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(output_dir, "tables"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(output_dir, "figs"),   showWarnings = FALSE, recursive = TRUE)

# ── Cargar datos ──────────────────────────────────────────────────────────────
# dt <- readRDS("dt_con_cs_nestedness.rds")
setDT(dt)

needed <- c("diffusion", "wage_gap", "structural_distance",
            "domain", "source", "target", "skill_name")
dt[, setdiff(names(dt), needed) := NULL]
gc(); gc(); gc()

# Variables derivadas
dt[, wage_up   := pmax(0,  wage_gap)]
dt[, wage_down := pmin(0,  wage_gap)]
dt[, up_dummy  := fifelse(wage_gap > 0, 1L, 0L)]
dt[, domain    := factor(domain, levels = c("Cognitive", "Physical"))]
gc(); gc(); gc()

# ── Muestreo por source occupation ───────────────────────────────────────────
SAMPLE_FRAC  <- 0.50
SEED         <- 42L
ckpt_sources <- file.path(output_dir, "rds", "sources_sample_seed42.rds")

if (file.exists(ckpt_sources)) {
  sources_sample <- readRDS(ckpt_sources)
  message("Sources cargadas desde disco: ", length(sources_sample))
} else {
  set.seed(SEED)
  sources_all    <- unique(dt$source)
  sources_sample <- sample(sources_all,
                           size = round(length(sources_all) * SAMPLE_FRAC))
  saveRDS(sources_sample, ckpt_sources)
  message("Sources generadas: ", length(sources_sample),
          " de ", length(sources_all), " (", round(SAMPLE_FRAC*100), "%)")
}

dt_sample <- dt[source %in% sources_sample]
message("Tríadas en muestra: ", format(nrow(dt_sample), big.mark = ","))
message("Sources: ", uniqueN(dt_sample$source),
        " | Targets: ", uniqueN(dt_sample$target),
        " | Skills: ", uniqueN(dt_sample$skill_name))

# ── wage_q_source: quintil salarial de la OCUPACIÓN FUENTE ───────────────────
# wage_gap = log_wage_target - log_wage_source, es propiedad de la díada (j,i).
# Necesitamos un atributo de la SOURCE j, no de la díada.
# Calculamos mean(wage_gap) por source: dado que log_wage_source es constante
# para una source fija, mean_j(wage_gap_ji) = mean_j(log_wage_i) - log_wage_j.
# Si mean_j(log_wage_i) es aproximadamente constante entre sources (razonable
# con un skillscape denso), esto es un proxy válido de -log_wage_source.
# El quintil 5 = sources de mayor salario (wage_gap más negativo en promedio).
source_wage <- dt_sample[, .(mean_wg = mean(wage_gap, na.rm = TRUE)),
                          by = source]
source_wage[, wage_q_source := as.integer(cut(
  mean_wg,
  breaks         = quantile(mean_wg, probs = 0:5/5, na.rm = TRUE),
  labels         = 1:5,
  include.lowest = TRUE
))]
dt_sample[source_wage, on = "source", wage_q_source := i.wage_q_source]
rm(source_wage)

# Verificaciones
stopifnot("wage_q_source no debe tener NAs" = !any(is.na(dt_sample$wage_q_source)))
check_q <- dt_sample[, .(n_q = uniqueN(wage_q_source)), by = source]
stopifnot("Cada source debe tener exactamente un quintil" = all(check_q$n_q == 1))
rm(check_q); gc()

message("wage_q_source OK (atributo de source, no de díada).")
message("Distribución de quintiles:")
print(dt_sample[, .N, keyby = wage_q_source])

# ── Fórmula canónica ─────────────────────────────────────────────────────────
fml_rhs <- diffusion ~
  (up_dummy + wage_up + wage_down + structural_distance) : domain

# ── Extractor de coeficientes ─────────────────────────────────────────────────
extract_coefs <- function(m, panel_short) {
  ct <- coeftable(m)
  rn <- rownames(ct)

  find_one <- function(pats) {
    for (p in pats) {
      h <- grep(p, rn, value = TRUE)[1L]
      if (!is.na(h)) return(c(ct[h, "Estimate"], ct[h, "Std. Error"]))
    }
    c(NA_real_, NA_real_)
  }

  params <- list(
    Theta_up_Cog = find_one(c("wage_up:domainCognitive",
                               "domainCognitive:wage_up")),
    Theta_dn_Cog = find_one(c("wage_down:domainCognitive",
                               "domainCognitive:wage_down")),
    kappa_Cog    = find_one(c("up_dummy:domainCognitive",
                               "domainCognitive:up_dummy")),
    delta_Cog    = find_one(c("structural_distance:domainCognitive",
                               "domainCognitive:structural_distance")),
    Theta_up_Phy = find_one(c("wage_up:domainPhysical",
                               "domainPhysical:wage_up")),
    Theta_dn_Phy = find_one(c("wage_down:domainPhysical",
                               "domainPhysical:wage_down")),
    kappa_Phy    = find_one(c("up_dummy:domainPhysical",
                               "domainPhysical:up_dummy")),
    delta_Phy    = find_one(c("structural_distance:domainPhysical",
                               "domainPhysical:structural_distance"))
  )

  data.table(
    panel_short = panel_short,
    coef        = names(params),
    estimate    = vapply(params, `[`, numeric(1), 1),
    se          = vapply(params, `[`, numeric(1), 2)
  )
}

message("Setup completo. RAM:"); print(gc())
