# =============================================================================
# 06_subsample_stability.R
# Estabilidad del subsample de source occupations — 3 seeds × 2 paneles
#
# QUÉ TESTEA:
#   Dado que todos los análisis usan 50% de las source occupations, demostramos
#   que los coeficientes ATC no dependen del draw específico. Corremos el
#   baseline con seeds 42 (ya existe), 123 y 999.
#   Si el coeficiente de variación entre seeds es < 5%, el subsample es estable.
#   Esto va en Supplementary Table SX y cierra el argumento de que el 50%
#   es metodológicamente suficiente.
#
# ESTRUCTURA TRIÁDICA:
#   Cada seed selecciona un conjunto diferente de source occupations.
#   Dentro de cada conjunto, todas las tríadas de esas sources se retienen.
#   La estructura triádica se preserva igual que en el baseline principal.
#
# SESIONES REQUERIDAS (una por modelo, cerrar R entre cada una):
#   Sesión 1: seed=123, Panel A → corre BLOQUE_123A
#   Sesión 2: seed=123, Panel B → corre BLOQUE_123B
#   Sesión 3: seed=999, Panel A → corre BLOQUE_999A
#   Sesión 4: seed=999, Panel B → corre BLOQUE_999B
#   Sesión 5: consolidar        → corre BLOQUE_CONSOLIDAR (sin dt, sin modelos)
#
# El seed=42 (Panel A y Panel B) ya están en baseline_coefs.rds
#
# ANTES DE CORRER SEED=999: verificar que el archivo seed999.rds NO exista.
#   Si existe (porque se copió accidentalmente de seed123), eliminarlo con:
#   file.remove("output_SI_identity/rds/sources_sample_seed999.rds")
# =============================================================================


# =============================================================================
# BLOQUE_123A — seed=123, Panel A
# SESIÓN LIMPIA. Cerrar R al terminar.
# =============================================================================

library(data.table)
library(fixest)

output_dir <- "output_SI_identity"
dir.create(file.path(output_dir, "rds"),    showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(output_dir, "tables"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(output_dir, "figs"),   showWarnings = FALSE, recursive = TRUE)

dt <- readRDS("dt_con_cs_nestedness.rds")
setDT(dt)

needed <- c("diffusion", "wage_gap", "structural_distance",
            "domain", "source", "target", "skill_name")
dt[, setdiff(names(dt), needed) := NULL]
gc(); gc(); gc()

dt[, wage_up   := pmax(0,  wage_gap)]
dt[, wage_down := pmin(0,  wage_gap)]
dt[, up_dummy  := fifelse(wage_gap > 0, 1L, 0L)]
dt[, domain    := factor(domain, levels = c("Cognitive", "Physical"))]
gc(); gc(); gc()

SAMPLE_FRAC  <- 0.50
SEED         <- 123L
ckpt_sources <- file.path(output_dir, "rds", "sources_sample_seed123.rds")

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
          " de ", length(sources_all), " (", round(SAMPLE_FRAC * 100), "%)")
}

dt_sample <- dt[source %in% sources_sample]
message("Tríadas en muestra: ", format(nrow(dt_sample), big.mark = ","))
message("Sources: ", uniqueN(dt_sample$source),
        " | Targets: ", uniqueN(dt_sample$target),
        " | Skills: ", uniqueN(dt_sample$skill_name))

source_wage <- dt_sample[, .(mean_wg = mean(wage_gap, na.rm = TRUE)), by = source]
source_wage[, wage_q_source := as.integer(cut(
  mean_wg,
  breaks         = quantile(mean_wg, probs = 0:5 / 5, na.rm = TRUE),
  labels         = 1:5,
  include.lowest = TRUE
))]
dt_sample[source_wage, on = "source", wage_q_source := i.wage_q_source]
rm(source_wage)

stopifnot("wage_q_source no debe tener NAs" = !any(is.na(dt_sample$wage_q_source)))
check_q <- dt_sample[, .(n_q = uniqueN(wage_q_source)), by = source]
stopifnot("Cada source debe tener exactamente un quintil" = all(check_q$n_q == 1))
rm(check_q); gc()

fml_rhs <- diffusion ~
  (up_dummy + wage_up + wage_down + structural_distance) : domain

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
    Theta_up_Cog = find_one(c("wage_up:domainCognitive",        "domainCognitive:wage_up")),
    Theta_dn_Cog = find_one(c("wage_down:domainCognitive",      "domainCognitive:wage_down")),
    kappa_Cog    = find_one(c("up_dummy:domainCognitive",       "domainCognitive:up_dummy")),
    delta_Cog    = find_one(c("structural_distance:domainCognitive", "domainCognitive:structural_distance")),
    Theta_up_Phy = find_one(c("wage_up:domainPhysical",         "domainPhysical:wage_up")),
    Theta_dn_Phy = find_one(c("wage_down:domainPhysical",       "domainPhysical:wage_down")),
    kappa_Phy    = find_one(c("up_dummy:domainPhysical",        "domainPhysical:up_dummy")),
    delta_Phy    = find_one(c("structural_distance:domainPhysical", "domainPhysical:structural_distance"))
  )
  data.table(
    panel_short = panel_short,
    coef        = names(params),
    estimate    = vapply(params, `[`, numeric(1), 1),
    se          = vapply(params, `[`, numeric(1), 2)
  )
}

message("Setup completo. RAM:"); print(gc())

# ── Modelo ────────────────────────────────────────────────────────────────────
message("Estabilidad seed=123, Panel A...")

m <- feglm(fml_rhs,
           data    = dt_sample,
           family  = binomial("cloglog"),
           fixef   = c("source", "skill_name"),
           cluster = c("source", "target", "skill_name"),
           lean = TRUE, mem.clean = TRUE, nthreads = 0)

res <- extract_coefs(m, "Panel A")
res[, seed := 123L]
rm(m); gc(); gc(); gc()

saveRDS(res, file.path(output_dir, "rds", "stability_seed123_A.rds"))
print(res)
message("Guardado. CERRAR R.")


# =============================================================================
# BLOQUE_123B — seed=123, Panel B
# SESIÓN LIMPIA. Cerrar R al terminar.
# =============================================================================

library(data.table)
library(fixest)

output_dir <- "output_SI_identity"
dir.create(file.path(output_dir, "rds"),    showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(output_dir, "tables"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(output_dir, "figs"),   showWarnings = FALSE, recursive = TRUE)

dt <- readRDS("dt_con_cs_nestedness.rds")
setDT(dt)

needed <- c("diffusion", "wage_gap", "structural_distance",
            "domain", "source", "target", "skill_name")
dt[, setdiff(names(dt), needed) := NULL]
gc(); gc(); gc()

dt[, wage_up   := pmax(0,  wage_gap)]
dt[, wage_down := pmin(0,  wage_gap)]
dt[, up_dummy  := fifelse(wage_gap > 0, 1L, 0L)]
dt[, domain    := factor(domain, levels = c("Cognitive", "Physical"))]
gc(); gc(); gc()

SAMPLE_FRAC  <- 0.50
SEED         <- 123L
ckpt_sources <- file.path(output_dir, "rds", "sources_sample_seed123.rds")

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
          " de ", length(sources_all), " (", round(SAMPLE_FRAC * 100), "%)")
}

dt_sample <- dt[source %in% sources_sample]

source_wage <- dt_sample[, .(mean_wg = mean(wage_gap, na.rm = TRUE)), by = source]
source_wage[, wage_q_source := as.integer(cut(
  mean_wg,
  breaks         = quantile(mean_wg, probs = 0:5 / 5, na.rm = TRUE),
  labels         = 1:5,
  include.lowest = TRUE
))]
dt_sample[source_wage, on = "source", wage_q_source := i.wage_q_source]
rm(source_wage)

stopifnot(!any(is.na(dt_sample$wage_q_source)))
check_q <- dt_sample[, .(n_q = uniqueN(wage_q_source)), by = source]
stopifnot(all(check_q$n_q == 1))
rm(check_q); gc()

fml_rhs <- diffusion ~
  (up_dummy + wage_up + wage_down + structural_distance) : domain

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
    Theta_up_Cog = find_one(c("wage_up:domainCognitive",        "domainCognitive:wage_up")),
    Theta_dn_Cog = find_one(c("wage_down:domainCognitive",      "domainCognitive:wage_down")),
    kappa_Cog    = find_one(c("up_dummy:domainCognitive",       "domainCognitive:up_dummy")),
    delta_Cog    = find_one(c("structural_distance:domainCognitive", "domainCognitive:structural_distance")),
    Theta_up_Phy = find_one(c("wage_up:domainPhysical",         "domainPhysical:wage_up")),
    Theta_dn_Phy = find_one(c("wage_down:domainPhysical",       "domainPhysical:wage_down")),
    kappa_Phy    = find_one(c("up_dummy:domainPhysical",        "domainPhysical:up_dummy")),
    delta_Phy    = find_one(c("structural_distance:domainPhysical", "domainPhysical:structural_distance"))
  )
  data.table(
    panel_short = panel_short,
    coef        = names(params),
    estimate    = vapply(params, `[`, numeric(1), 1),
    se          = vapply(params, `[`, numeric(1), 2)
  )
}

message("Setup completo. RAM:"); print(gc())

# ── Modelo ────────────────────────────────────────────────────────────────────
message("Estabilidad seed=123, Panel B...")

m <- feglm(fml_rhs,
           data    = dt_sample,
           family  = binomial("cloglog"),
           fixef   = c("target", "skill_name"),
           cluster = c("source", "target", "skill_name"),
           lean = TRUE, mem.clean = TRUE, nthreads = 0)

res <- extract_coefs(m, "Panel B")
res[, seed := 123L]
rm(m); gc(); gc(); gc()

saveRDS(res, file.path(output_dir, "rds", "stability_seed123_B.rds"))
print(res)
message("Guardado. CERRAR R.")


# =============================================================================
# BLOQUE_999A — seed=999, Panel A
# SESIÓN LIMPIA. Cerrar R al terminar.
#
# IMPORTANTE: Si el archivo sources_sample_seed999.rds existe de una ejecución
# fallida anterior, eliminarlo primero:
#   file.remove("output_SI_identity/rds/sources_sample_seed999.rds")
# =============================================================================

library(data.table)
library(fixest)

output_dir <- "output_SI_identity"
dir.create(file.path(output_dir, "rds"),    showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(output_dir, "tables"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(output_dir, "figs"),   showWarnings = FALSE, recursive = TRUE)

dt <- readRDS("dt_con_cs_nestedness.rds")
setDT(dt)

needed <- c("diffusion", "wage_gap", "structural_distance",
            "domain", "source", "target", "skill_name")
dt[, setdiff(names(dt), needed) := NULL]
gc(); gc(); gc()

dt[, wage_up   := pmax(0,  wage_gap)]
dt[, wage_down := pmin(0,  wage_gap)]
dt[, up_dummy  := fifelse(wage_gap > 0, 1L, 0L)]
dt[, domain    := factor(domain, levels = c("Cognitive", "Physical"))]
gc(); gc(); gc()

SAMPLE_FRAC  <- 0.50
SEED         <- 999L
ckpt_sources <- file.path(output_dir, "rds", "sources_sample_seed999.rds")

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
          " de ", length(sources_all), " (", round(SAMPLE_FRAC * 100), "%)")
}

dt_sample <- dt[source %in% sources_sample]
message("Tríadas en muestra: ", format(nrow(dt_sample), big.mark = ","))
message("Sources: ", uniqueN(dt_sample$source),
        " | Targets: ", uniqueN(dt_sample$target),
        " | Skills: ", uniqueN(dt_sample$skill_name))

source_wage <- dt_sample[, .(mean_wg = mean(wage_gap, na.rm = TRUE)), by = source]
source_wage[, wage_q_source := as.integer(cut(
  mean_wg,
  breaks         = quantile(mean_wg, probs = 0:5 / 5, na.rm = TRUE),
  labels         = 1:5,
  include.lowest = TRUE
))]
dt_sample[source_wage, on = "source", wage_q_source := i.wage_q_source]
rm(source_wage)

stopifnot(!any(is.na(dt_sample$wage_q_source)))
check_q <- dt_sample[, .(n_q = uniqueN(wage_q_source)), by = source]
stopifnot(all(check_q$n_q == 1))
rm(check_q); gc()

fml_rhs <- diffusion ~
  (up_dummy + wage_up + wage_down + structural_distance) : domain

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
    Theta_up_Cog = find_one(c("wage_up:domainCognitive",        "domainCognitive:wage_up")),
    Theta_dn_Cog = find_one(c("wage_down:domainCognitive",      "domainCognitive:wage_down")),
    kappa_Cog    = find_one(c("up_dummy:domainCognitive",       "domainCognitive:up_dummy")),
    delta_Cog    = find_one(c("structural_distance:domainCognitive", "domainCognitive:structural_distance")),
    Theta_up_Phy = find_one(c("wage_up:domainPhysical",         "domainPhysical:wage_up")),
    Theta_dn_Phy = find_one(c("wage_down:domainPhysical",       "domainPhysical:wage_down")),
    kappa_Phy    = find_one(c("up_dummy:domainPhysical",        "domainPhysical:up_dummy")),
    delta_Phy    = find_one(c("structural_distance:domainPhysical", "domainPhysical:structural_distance"))
  )
  data.table(
    panel_short = panel_short,
    coef        = names(params),
    estimate    = vapply(params, `[`, numeric(1), 1),
    se          = vapply(params, `[`, numeric(1), 2)
  )
}

message("Setup completo. RAM:"); print(gc())

# ── Modelo ────────────────────────────────────────────────────────────────────
message("Estabilidad seed=999, Panel A...")

m <- feglm(fml_rhs,
           data    = dt_sample,
           family  = binomial("cloglog"),
           fixef   = c("source", "skill_name"),
           cluster = c("source", "target", "skill_name"),
           lean = TRUE, mem.clean = TRUE, nthreads = 0)

res <- extract_coefs(m, "Panel A")
res[, seed := 999L]
rm(m); gc(); gc(); gc()

saveRDS(res, file.path(output_dir, "rds", "stability_seed999_A.rds"))
print(res)
message("Guardado. CERRAR R.")


# =============================================================================
# BLOQUE_999B — seed=999, Panel B
# SESIÓN LIMPIA. Cerrar R al terminar.
# =============================================================================

library(data.table)
library(fixest)

output_dir <- "output_SI_identity"
dir.create(file.path(output_dir, "rds"),    showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(output_dir, "tables"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(output_dir, "figs"),   showWarnings = FALSE, recursive = TRUE)

dt <- readRDS("dt_con_cs_nestedness.rds")
setDT(dt)

needed <- c("diffusion", "wage_gap", "structural_distance",
            "domain", "source", "target", "skill_name")
dt[, setdiff(names(dt), needed) := NULL]
gc(); gc(); gc()

dt[, wage_up   := pmax(0,  wage_gap)]
dt[, wage_down := pmin(0,  wage_gap)]
dt[, up_dummy  := fifelse(wage_gap > 0, 1L, 0L)]
dt[, domain    := factor(domain, levels = c("Cognitive", "Physical"))]
gc(); gc(); gc()

SAMPLE_FRAC  <- 0.50
SEED         <- 999L
ckpt_sources <- file.path(output_dir, "rds", "sources_sample_seed999.rds")

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
          " de ", length(sources_all), " (", round(SAMPLE_FRAC * 100), "%)")
}

dt_sample <- dt[source %in% sources_sample]

source_wage <- dt_sample[, .(mean_wg = mean(wage_gap, na.rm = TRUE)), by = source]
source_wage[, wage_q_source := as.integer(cut(
  mean_wg,
  breaks         = quantile(mean_wg, probs = 0:5 / 5, na.rm = TRUE),
  labels         = 1:5,
  include.lowest = TRUE
))]
dt_sample[source_wage, on = "source", wage_q_source := i.wage_q_source]
rm(source_wage)

stopifnot(!any(is.na(dt_sample$wage_q_source)))
check_q <- dt_sample[, .(n_q = uniqueN(wage_q_source)), by = source]
stopifnot(all(check_q$n_q == 1))
rm(check_q); gc()

fml_rhs <- diffusion ~
  (up_dummy + wage_up + wage_down + structural_distance) : domain

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
    Theta_up_Cog = find_one(c("wage_up:domainCognitive",        "domainCognitive:wage_up")),
    Theta_dn_Cog = find_one(c("wage_down:domainCognitive",      "domainCognitive:wage_down")),
    kappa_Cog    = find_one(c("up_dummy:domainCognitive",       "domainCognitive:up_dummy")),
    delta_Cog    = find_one(c("structural_distance:domainCognitive", "domainCognitive:structural_distance")),
    Theta_up_Phy = find_one(c("wage_up:domainPhysical",         "domainPhysical:wage_up")),
    Theta_dn_Phy = find_one(c("wage_down:domainPhysical",       "domainPhysical:wage_down")),
    kappa_Phy    = find_one(c("up_dummy:domainPhysical",        "domainPhysical:up_dummy")),
    delta_Phy    = find_one(c("structural_distance:domainPhysical", "domainPhysical:structural_distance"))
  )
  data.table(
    panel_short = panel_short,
    coef        = names(params),
    estimate    = vapply(params, `[`, numeric(1), 1),
    se          = vapply(params, `[`, numeric(1), 2)
  )
}

message("Setup completo. RAM:"); print(gc())

# ── Modelo ────────────────────────────────────────────────────────────────────
message("Estabilidad seed=999, Panel B...")

m <- feglm(fml_rhs,
           data    = dt_sample,
           family  = binomial("cloglog"),
           fixef   = c("target", "skill_name"),
           cluster = c("source", "target", "skill_name"),
           lean = TRUE, mem.clean = TRUE, nthreads = 0)

res <- extract_coefs(m, "Panel B")
res[, seed := 999L]
rm(m); gc(); gc(); gc()

saveRDS(res, file.path(output_dir, "rds", "stability_seed999_B.rds"))
print(res)
message("Guardado. CERRAR R.")


# =============================================================================
# BLOQUE_CONSOLIDAR — sin modelos, sin dt
# Correr en sesión limpia DESPUÉS de los 4 bloques anteriores.
# =============================================================================

library(data.table)
library(ggplot2)

output_dir <- "output_SI_identity"
ATC_COEFS  <- c("Theta_up_Cog", "Theta_dn_Cog", "Theta_up_Phy", "Theta_dn_Phy")

# Cargar los 6 resultados (seed42 del baseline, seeds 123 y 999 de este script)
base   <- readRDS(file.path(output_dir, "rds", "baseline_coefs.rds"))
base[, seed := 42L]

s123_A <- readRDS(file.path(output_dir, "rds", "stability_seed123_A.rds"))
s123_B <- readRDS(file.path(output_dir, "rds", "stability_seed123_B.rds"))
s999_A <- readRDS(file.path(output_dir, "rds", "stability_seed999_A.rds"))
s999_B <- readRDS(file.path(output_dir, "rds", "stability_seed999_B.rds"))

all_stab <- rbind(base, s123_A, s123_B, s999_A, s999_B, fill = TRUE)

# Tabla: coeficientes por seed, separado por panel
stab_wide <- dcast(
  all_stab[coef %in% ATC_COEFS],
  coef + panel_short ~ seed,
  value.var = "estimate"
)
setnames(stab_wide,
         c("42",     "123",      "999"),
         c("est_42", "est_123",  "est_999"))

# Coeficiente de variación entre los 3 seeds
stab_wide[, mean_est := rowMeans(.SD, na.rm = TRUE),
          .SDcols = c("est_42", "est_123", "est_999")]
stab_wide[, sd_est   := apply(.SD, 1, sd, na.rm = TRUE),
          .SDcols = c("est_42", "est_123", "est_999")]
stab_wide[, cv       := sd_est / abs(mean_est)]
stab_wide[, stable   := cv < 0.05]

fwrite(stab_wide,
       file.path(output_dir, "tables", "table_SI_subsample_stability.csv"))

cat("\n====== ESTABILIDAD DEL SUBSAMPLE ======\n")
print(stab_wide)
cat("\nTodos los coef ATC estables (CV < 5%):",
    all(stab_wide$stable, na.rm = TRUE), "\n")

# Figura de estabilidad
stab_long <- melt(stab_wide,
                  id.vars      = c("coef", "panel_short", "mean_est", "cv"),
                  measure.vars = c("est_42", "est_123", "est_999"),
                  variable.name = "seed_label",
                  value.name    = "estimate")
stab_long[, seed_label := gsub("est_", "seed=", seed_label)]

coef_labels <- c(
  Theta_up_Cog = "\u0398\u2191 Cognitive",
  Theta_dn_Cog = "\u0398\u2193 Cognitive",
  Theta_up_Phy = "\u0398\u2191 Physical",
  Theta_dn_Phy = "\u0398\u2193 Physical"
)
stab_long[, coef_label := coef_labels[coef]]

p_stab <- ggplot(stab_long,
  aes(x = seed_label, y = estimate,
      color = panel_short, group = panel_short)) +
  geom_hline(yintercept = 0, color = "gray70", linewidth = .3) +
  geom_line(linewidth = .6) +
  geom_point(size = 2.5) +
  facet_wrap(~ coef_label, scales = "free_y", ncol = 2) +
  scale_color_manual(values = c("Panel A" = "#1f6b8e", "Panel B" = "#c0392b")) +
  labs(title    = "Fig. S[X]. Subsample stability across random draws",
       subtitle = "Three independent 50% subsamples of source occupations",
       x        = "Random draw (seed)",
       y        = "Estimate (complementary log-log scale)",
       color    = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold"))

ggsave(file.path(output_dir, "figs", "fig_SI_subsample_stability.png"),
       p_stab, width = 10, height = 8, dpi = 300)

message("Estabilidad consolidada. Outputs → ", normalizePath(output_dir))


# Criterio mixto: estable si CV < 5% O SD absoluta < 0.02
stab_wide[, stable := cv < 0.05 | sd_est < 0.02]
