# ==============================================================================
# SCRIPT MAESTRO: ANÁLISIS DE SUB-PERÍODOS — TABLE S2
# Paper: Asymmetric Trajectory Channeling (ATC)
# ==============================================================================
# Lógica:
#   - cs_threshold y clasificación de archetypes: predeterminados desde 2015
#   - Dyads construidos por separado para cada sub-período
#   - Panel A (source + skill FE) y Panel B (target + skill FE) por período
#   - Output: Table S2 con β↑ y β↓ por archetype × sub-período
#
# Sub-períodos:
#   2015–2018: db_15_1      → db_23_1_text
#   2019–2021: db_24_1_text → db_26_1_text
#   2022–2024: db_27_1_text → db_29_2_text
# ==============================================================================

gc()
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  data.table, readxl, progress, Matrix, lsa,
  stringr, dplyr, fixest, broom
)

sub_periods <- list(
  list(label   = "2015-2018",
       path_t0 = "data/db_15_1",
       path_t1 = "data/db_23_1_text"),
  list(label   = "2019-2021",
       path_t0 = "data/db_24_1_text",
       path_t1 = "data/db_26_1_text"),
  list(label   = "2022-2024",
       path_t0 = "data/db_27_1_text",
       path_t1 = "data/db_29_2_text")
)

crosswalk_data_path <- "data/crosswalk/"
data_external_path  <- "data/"
file_bls_15         <- "national_M2015_dl.xlsx"
file_nestedness     <- "dt_con_cs_nestedness.rds"
file_cw_soc10_19    <- "2010_to_2019_Crosswalk.csv"
output_dir          <- "subperiod_results"

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

message("\n>>> 1. CARGANDO NESTEDNESS Y DEFINIENDO ARCHETYPES (2015)...")

dt_nest_full <- readRDS(file_nestedness)
setDT(dt_nest_full)

cs_threshold <- dt_nest_full[domain == "Cognitive", median(cs, na.rm = TRUE)]
message("   cs_threshold (mediana Cognitive, 2015) = ", round(cs_threshold, 4))

skill_lookup <- unique(dt_nest_full[, .(skill_name, domain, cs)])
rm(dt_nest_full); gc()

clean_id <- function(x) {
  s <- as.character(x)
  s <- gsub("-", "", s)
  s <- gsub("\\.00$", "", s)
  s <- gsub("\\.", "", s)
  return(stringr::str_trim(s))
}

load_onet <- function(folder, filename) {
  f <- file.path(folder, filename)
  if (!file.exists(f)) {
    pat <- gsub(".txt", "", filename, fixed = TRUE)
    f_alt <- list.files(folder, pattern = pat, full.names = TRUE)
    if (length(f_alt) > 0) f <- f_alt[1] else {
      message("   ⚠️  No encontrado: ", filename, " en ", folder)
      return(NULL)
    }
  }
  dt <- fread(f, sep = "\t", quote = "",
              na.strings = c("NA", "n/a", "", "*"),
              showProgress = FALSE)
  setnames(dt, names(dt), make.names(names(dt)))
  if ("O.NET.SOC.Code" %in% names(dt))
    dt[, soc_code := clean_id(O.NET.SOC.Code)]
  val_col <- grep("Data.?Value", names(dt), value = TRUE)[1]
  if (!is.na(val_col)) dt[, value := as.numeric(get(val_col))]
  return(dt)
}

calc_rca <- function(dt_input) {
  agg <- dt_input[, .(v = mean(value, na.rm = TRUE)),
                  by = .(soc_code, Element.Name)]
  occ_sum <- agg[, .(ot = sum(v)), by = soc_code]
  ski_sum <- agg[, .(st = sum(v)), by = Element.Name]
  gt <- sum(agg$v)
  agg <- merge(agg, occ_sum, by = "soc_code")
  agg <- merge(agg, ski_sum, by = "Element.Name")
  agg[, rca := (v / ot) / (st / gt)]
  return(agg[, .(soc_code, skill = Element.Name, rca)])
}

build_dyads <- function(path_t0, path_t1, label, w15, skill_lookup,
                        cw_clean, cs_threshold) {

  message("\n   --- Sub-período: ", label, " ---")
  files <- c("Skills.txt", "Abilities.txt", "Knowledge.txt", "Work Activities.txt")

  # ---- t0 ----
  dt_t0_list <- lapply(files, function(x) load_onet(path_t0, x))
  dt_t0_list <- dt_t0_list[!sapply(dt_t0_list, is.null)]
  dt_t0 <- rbindlist(dt_t0_list, fill = TRUE)
  dt_t0 <- dt_t0[Scale.ID == "IM" & !is.na(value)]

  if (grepl("db_15", path_t0) && !is.null(cw_clean)) {
    dt_t0 <- merge(dt_t0, cw_clean,
                   by.x = "soc_code", by.y = "soc10",
                   all.x = FALSE, allow.cartesian = TRUE)
    dt_t0[, soc_code := soc19]
  }

  rca_t0 <- calc_rca(dt_t0)
  setnames(rca_t0, "rca", "rca_t0")
  rm(dt_t0, dt_t0_list); gc()

  # ---- t1 ----
  dt_t1_list <- lapply(files, function(x) load_onet(path_t1, x))
  dt_t1_list <- dt_t1_list[!sapply(dt_t1_list, is.null)]
  dt_t1 <- rbindlist(dt_t1_list, fill = TRUE)
  dt_t1 <- dt_t1[Scale.ID == "IM" & !is.na(value)]
  rca_t1 <- calc_rca(dt_t1)
  setnames(rca_t1, "rca", "rca_t1")
  rm(dt_t1, dt_t1_list); gc()

  # ---- Distancias estructurales (cosine ponderado desde t0 del sub-período) ----
  mat_dt <- rca_t0[rca_t0 > 1]
  occ_wide_wt <- dcast(mat_dt, soc_code ~ skill,
                        fun.aggregate = mean, value.var = "rca_t0")
  occ_ids <- occ_wide_wt$soc_code
  occ_mat  <- as.matrix(occ_wide_wt[, -1])
  occ_mat[is.na(occ_mat)] <- 0
  M_wt <- Matrix::Matrix(occ_mat, sparse = TRUE)

  dot_wt   <- Matrix::tcrossprod(M_wt)
  norm_wt  <- sqrt(Matrix::rowSums(M_wt^2))
  norm_vec <- as.numeric(norm_wt); names(norm_vec) <- occ_ids

  dotw_sum <- Matrix::summary(dot_wt)
  dist_dt  <- data.table(
    source = occ_ids[dotw_sum$i],
    target = occ_ids[dotw_sum$j],
    dot    = as.numeric(dotw_sum$x)
  )[source != target]

  dist_dt[, cosine_sim := dot / (norm_vec[source] * norm_vec[target])]
  dist_dt <- dist_dt[is.finite(cosine_sim) & cosine_sim > 0]
  dist_dt[, structural_distance := 1 - cosine_sim]
  dist_dt[, c("dot", "cosine_sim") := NULL]

  rm(mat_dt, occ_wide_wt, occ_mat, M_wt, dot_wt, norm_wt, dotw_sum); gc()

  # ---- Risk set y outcome ----
  panel <- merge(rca_t0, rca_t1, by = c("soc_code", "skill"), all = TRUE)
  panel[is.na(rca_t0), rca_t0 := 0]
  panel[is.na(rca_t1), rca_t1 := 0]

  sources_t0   <- panel[rca_t0 > 1.0, .(source = soc_code, skill)]
  targets_risk <- panel[rca_t0 <= 1.0, .(target = soc_code, skill, rca_t1)]
  targets_risk[, diffusion := fifelse(rca_t1 > 1.0, 1L, 0L)]

  # 50% subsample de fuentes
  set.seed(42)
  srcs_all   <- unique(sources_t0$source)
  srcs_sub   <- sample(srcs_all, floor(length(srcs_all) * 0.5))
  sources_t0 <- sources_t0[source %in% srcs_sub]

  all_dyads_list <- list()
  skills_vec <- unique(sources_t0$skill)
  pb <- progress::progress_bar$new(total = length(skills_vec))

  for (sk in skills_vec) {
    pb$tick()
    srcs <- sources_t0[skill == sk]$source
    tgts <- targets_risk[skill == sk]
    if (length(srcs) == 0 || nrow(tgts) == 0) next

    pos <- tgts[diffusion == 1]
    neg <- tgts[diffusion == 0]
    if (nrow(neg) > 2000) neg <- neg[sample(.N, 2000)]
    tgts_final <- rbind(pos, neg)

    pairs <- as.data.table(expand.grid(
      source = srcs, target = tgts_final$target,
      stringsAsFactors = FALSE
    ))
    pairs <- pairs[source != target]
    pairs <- merge(pairs, tgts_final[, .(target, diffusion)], by = "target")
    pairs[, skill_name := sk]
    all_dyads_list[[sk]] <- pairs
  }

  all_events <- rbindlist(all_dyads_list)
  rm(all_dyads_list, panel, sources_t0, targets_risk, rca_t0, rca_t1); gc()

  # ---- Salarios (predeterminados 2015) ----
  s_wage <- copy(w15)
  setnames(s_wage, c("c","wage","log_w"), c("source","s_wage","log_s_wage"))
  t_wage <- copy(w15)
  setnames(t_wage, c("c","wage","log_w"), c("target","t_wage","log_t_wage"))
  all_events <- merge(all_events, s_wage, by = "source", all.x = TRUE)
  all_events <- merge(all_events, t_wage, by = "target", all.x = TRUE)

  all_events[, wage_gap  := log_t_wage - log_s_wage]
  all_events[, wage_up   := pmax(0, wage_gap)]
  all_events[, wage_down := pmin(0, wage_gap)]
  all_events[, up_dummy  := fifelse(wage_gap > 0, 1L, 0L)]

  setkey(all_events, source, target)
  setkey(dist_dt, source, target)
  all_events <- merge(all_events, dist_dt, all.x = TRUE)
  all_events[is.na(structural_distance), structural_distance := 1]
  rm(dist_dt); gc()

  all_events <- merge(all_events, skill_lookup, by = "skill_name", all.x = TRUE)

  all_events[, atc_archetype := fcase(
    domain == "Cognitive" & cs >= cs_threshold, "SC_Scaffolding",
    domain == "Cognitive" & cs <  cs_threshold, "SC_Specialized",
    domain == "Physical",                        "Physical_Terminal"
  )]
  all_events[, atc_archetype := factor(
    atc_archetype,
    levels = c("SC_Specialized", "SC_Scaffolding", "Physical_Terminal")
  )]

  all_events <- all_events[!is.na(atc_archetype) & !is.na(wage_gap)]
  return(all_events)
}

run_models <- function(dt_sub, label) {

  m_A <- feglm(
    diffusion ~ (wage_up + wage_down + up_dummy + structural_distance) : atc_archetype,
    data     = dt_sub,
    family   = binomial(link = "cloglog"),
    fixef    = c("source", "skill_name"),
    cluster  = c("source", "target", "skill_name"),
    nthreads = 0, mem.clean = TRUE, lean = TRUE
  )

  m_B <- feglm(
    diffusion ~ (wage_up + wage_down + up_dummy + structural_distance) : atc_archetype,
    data     = dt_sub,
    family   = binomial(link = "cloglog"),
    fixef    = c("target", "skill_name"),
    cluster  = c("source", "target", "skill_name"),
    nthreads = 0, mem.clean = TRUE, lean = TRUE
  )

  extract_coefs <- function(model, panel_label) {
    coef_dt <- as.data.table(broom::tidy(model, conf.int = TRUE))
    coef_dt <- coef_dt[grepl("wage_up|wage_down", term)]
    coef_dt[, archetype := fcase(
      grepl("SC_Scaffolding",    term), "SC_Scaffolding",
      grepl("SC_Specialized",    term), "SC_Specialized",
      grepl("Physical_Terminal", term), "Physical_Terminal"
    )]
    coef_dt[, direction := fifelse(grepl("wage_up", term), "beta_up", "beta_down")]
    coef_dt[, panel  := panel_label]
    coef_dt[, period := label]
    coef_dt[, .(period, panel, archetype, direction,
                estimate, std.error, conf.low, conf.high, p.value)]
  }

  rbind(extract_coefs(m_A, "Panel_A"),
        extract_coefs(m_B, "Panel_B"))
}

message("\n>>> 2. CARGANDO DATOS AUXILIARES...")

w15 <- {
  f <- file.path(data_external_path, file_bls_15)
  d <- as.data.table(readxl::read_excel(f, sheet = 1))
  cc <- grep("OCC_CODE", names(d), value = TRUE)[1]
  cw <- grep("A_MEDIAN", names(d), value = TRUE)[1]
  d[, w := suppressWarnings(as.numeric(as.character(get(cw))))]
  d[, c := clean_id(get(cc))]
  d[!is.na(w), .(wage = mean(w), log_w = log(mean(w))), by = c]
}

cw_clean <- NULL
cw_file  <- file.path(crosswalk_data_path, file_cw_soc10_19)
if (file.exists(cw_file)) {
  cw <- fread(cw_file)
  cw_clean <- unique(cw[, .(
    soc10 = clean_id(`O*NET-SOC 2010 Code`),
    soc19 = clean_id(`O*NET-SOC 2019 Code`)
  )])
}

all_results <- list()

for (sp in sub_periods) {

  dt_sub <- build_dyads(
    path_t0      = sp$path_t0,
    path_t1      = sp$path_t1,
    label        = sp$label,
    w15          = w15,
    skill_lookup = skill_lookup,
    cw_clean     = cw_clean,
    cs_threshold = cs_threshold
  )

  saveRDS(dt_sub, file.path(output_dir, paste0("dyads_", sp$label, ".rds")))

  coefs <- run_models(dt_sub, sp$label)
  all_results[[sp$label]] <- coefs

  saveRDS(coefs, file.path(output_dir, paste0("coefs_", sp$label, ".rds")))

  rm(dt_sub); gc(); gc()
}

results_all <- rbindlist(all_results)

saveRDS(results_all, file.path(output_dir, "table_S2_all_panels.rds"))
fwrite(results_all,  file.path(output_dir, "table_S2_all_panels.csv"))

message("\n>>> COMPLETADO. Resultados en: '", output_dir, "/'")
