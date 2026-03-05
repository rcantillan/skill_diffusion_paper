# ==============================================================================
# MASTER SCRIPT: SKILL DIFFUSION DATA CONSTRUCTION (V13) — MULTI-THRESHOLD
# ==============================================================================
# CAMBIOS RESPECTO A V12:
#   Se generan 4 columnas de outcome para el test de sensibilidad al umbral RCA:
#     - diffusion      : umbral 1.00 (baseline, idéntico a v12)
#     - diffusion_075  : umbral 0.75 (risk set: rca_t0_target < 0.75)
#     - diffusion_125  : umbral 1.25 (risk set: todos los dyads baseline)
#     - diffusion_150  : umbral 1.50 (risk set: todos los dyads baseline)
#
#   También se guardan:
#     - rca_t0_target  : RCA del target en t0 (para filtrar risk sets alternativos)
#     - rca_t1_target  : RCA del target en t1 (para auditoría)
#
# LÓGICA DE RISK SET:
#   El risk set BASE sigue siendo: source RCA > 1.0 AND target RCA <= 1.0 en t0.
#   Para umbrales > 1.0 (1.25, 1.50): el risk set base ya los cubre; solo cambia
#     el outcome (qué se considera "adoptado").
#   Para umbral < 1.0 (0.75): el risk set es MÁS RESTRICTIVO. En el script de
#     modelado, filtrar a rca_t0_target < 0.75 antes de estimar.
#
# NOTA: la fuente (source) siempre requiere RCA > 1.0 en t0, lo cual es más
#   estricto que 0.75, por lo que el lado source no cambia en ningún umbral.
# ==============================================================================

# --- 0. SETUP & LIBRERÍAS ---
gc()
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  data.table, readxl, progress, Matrix, lsa, cluster, igraph,
  stringr, dplyr, here
)

# --- 1. DEFINICIÓN DE RUTAS (AJUSTAR) ---
path_2015           <- "data/db_15_1"
path_2024           <- "data/db_29_2_text"
crosswalk_data_path <- "data/crosswalk/"
data_external_path  <- "data/"
output_data_dir     <- "datos_eventos_v13_MULTITHR"

if (!dir.exists(output_data_dir)) dir.create(output_data_dir, recursive = TRUE)

file_cw_soc10_19 <- "2010_to_2019_Crosswalk.csv"
file_bls_15      <- "national_M2015_dl.xlsx"

# Umbrales RCA para el test de sensibilidad
RCA_THRESHOLDS <- c(0.75, 1.00, 1.25, 1.50)

# ==============================================================================
# 2. FUNCIONES DE LIMPIEZA Y CARGA
# ==============================================================================

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
    pat <- gsub(".txt", "", filename)
    f_alt <- list.files(folder, pattern = pat, full.names = TRUE)
    if (length(f_alt) > 0) f <- f_alt[1] else return(NULL)
  }
  dt <- fread(f, sep = "\t", quote = "", na.strings = c("NA", "n/a", "", "*"),
              showProgress = FALSE)
  setnames(dt, old = names(dt), new = make.names(names(dt)))
  if ("O.NET.SOC.Code" %in% names(dt)) dt[, soc_code := clean_id(O.NET.SOC.Code)]
  val_col <- grep("Data.?Value", names(dt), value = TRUE)[1]
  if (!is.na(val_col)) dt[, value := as.numeric(get(val_col))]
  return(dt)
}

calc_rca <- function(dt) {
  agg     <- dt[, .(v = mean(value, na.rm = TRUE)), by = .(soc_code, Element.Name)]
  occ_sum <- agg[, .(ot = sum(v)), by = soc_code]
  ski_sum <- agg[, .(st = sum(v)), by = Element.Name]
  gt      <- sum(agg$v)
  agg <- merge(agg, occ_sum, by = "soc_code")
  agg <- merge(agg, ski_sum, by = "Element.Name")
  agg[, rca := (v / ot) / (st / gt)]
  return(agg[, .(soc_code, skill = Element.Name, rca)])
}

save_rds <- function(x, name) saveRDS(x, file.path(output_data_dir, name))

# ==============================================================================
# 3. PROCESAMIENTO BASELINE (2015 - t0)
# ==============================================================================
message("\n>>> 1. PROCESANDO 2015 (Baseline)...")

files <- c("Skills.txt", "Abilities.txt", "Knowledge.txt", "Work Activities.txt")
dt_15 <- rbindlist(lapply(files, function(x) load_onet(path_2015, x)), fill = TRUE)
dt_15 <- dt_15[Scale.ID == "IM" & !is.na(value)]

cw_file <- file.path(crosswalk_data_path, file_cw_soc10_19)
if (file.exists(cw_file)) {
  cw <- fread(cw_file)
  cw_clean <- unique(cw[, .(
    soc10 = clean_id(`O*NET-SOC 2010 Code`),
    soc19 = clean_id(`O*NET-SOC 2019 Code`)
  )])
  dt_15 <- merge(dt_15, cw_clean, by.x = "soc_code", by.y = "soc10",
                 all.x = FALSE, allow.cartesian = TRUE)
  dt_15[, soc_code := soc19]
} else {
  warning("⚠️ Crosswalk 2010-2019 no encontrado.")
}

rca_15 <- calc_rca(dt_15)
setnames(rca_15, "rca", "rca_t0")
message("Ocupaciones 2015 (Mapped): ", uniqueN(rca_15$soc_code))
rm(dt_15, cw, cw_clean); gc()

# ==============================================================================
# 4. PROCESAMIENTO OUTCOME (2024 - t1)
# ==============================================================================
message("\n>>> 2. PROCESANDO 2024 (Outcome)...")

dt_24 <- rbindlist(lapply(files, function(x) load_onet(path_2024, x)), fill = TRUE)
dt_24 <- dt_24[Scale.ID == "IM" & !is.na(value)]
rca_24 <- calc_rca(dt_24)
setnames(rca_24, "rca", "rca_t1")
rm(dt_24); gc()

# ==============================================================================
# 5. CLASIFICACIÓN & DISTANCIAS (ESTRUCTURA t0 — 2015)
# ==============================================================================
message("\n>>> 3. CALCULANDO ESTRUCTURA DE RED Y DISTANCIAS (2015)...")

mat_dt <- rca_15[rca_t0 > 1]
skill_classes     <- data.table(skill = unique(rca_15$skill), domain = "Unknown")
dist_dt           <- NULL
occ_edges_for_sp  <- NULL

if (nrow(mat_dt) > 0) {

  # --- A. Clusters de Skills (Louvain 2015) ---
  mat_wide <- dcast(mat_dt, skill ~ soc_code, value.var = "rca_t0", fun.aggregate = length)
  mat      <- as.matrix(mat_wide[, -1]); rownames(mat) <- mat_wide$skill

  sim_skill <- lsa::cosine(t(mat))
  sim_skill[is.nan(sim_skill)] <- 0; diag(sim_skill) <- 0
  g_skill <- graph_from_adjacency_matrix(sim_skill, mode = "undirected", weighted = TRUE)
  g_skill <- delete_edges(g_skill, E(g_skill)[weight < 0.1])
  lou     <- cluster_louvain(g_skill)

  skill_classes <- data.table(skill = lou$names,
                               cluster = paste0("C", lou$membership))
  skill_classes[, domain := fifelse(cluster == "C1", "Cognitive", "Physical")]

  # --- B. Matrices ocupación x skill (2015) ---
  message("   Construyendo matrices ocupación x skill (binaria y ponderada)...")

  occ_wide_bin <- dcast(mat_dt, soc_code ~ skill, fun.aggregate = length,
                        value.var = "rca_t0")
  occ_ids     <- occ_wide_bin$soc_code
  occ_mat_bin <- as.matrix(occ_wide_bin[, -1])
  occ_mat_bin <- (occ_mat_bin > 0) * 1L

  occ_wide_wt <- dcast(mat_dt, soc_code ~ skill, fun.aggregate = mean,
                       value.var = "rca_t0")
  occ_mat_wt  <- as.matrix(occ_wide_wt[, -1])
  occ_mat_wt[is.na(occ_mat_wt)] <- 0

  M_bin <- Matrix::Matrix(occ_mat_bin, sparse = TRUE)
  M_wt  <- Matrix::Matrix(occ_mat_wt,  sparse = TRUE)

  # Jaccard
  message("   Calculando Jaccard (binaria)...")
  inter_bin <- tcrossprod(M_bin)
  rs_bin    <- Matrix::rowSums(M_bin)
  inter_sum <- summary(inter_bin)
  j_dt <- data.table(
    source = occ_ids[inter_sum$i],
    target = occ_ids[inter_sum$j],
    inter  = as.numeric(inter_sum$x)
  )[source != target]
  rs_vec <- as.numeric(rs_bin); names(rs_vec) <- occ_ids
  j_dt[, union := rs_vec[source] + rs_vec[target] - inter]
  j_dt[, jaccard_sim  := fifelse(union > 0, inter / union, 0)]
  j_dt  <- j_dt[is.finite(jaccard_sim) & jaccard_sim > 0]
  j_dt[, jaccard_dist := 1 - jaccard_sim]
  j_dt[, c("inter", "union", "jaccard_sim") := NULL]

  # Cosine binaria
  message("   Calculando Cosine (binaria)...")
  cbin_dt <- data.table(
    source = occ_ids[inter_sum$i],
    target = occ_ids[inter_sum$j],
    dot    = as.numeric(inter_sum$x)
  )[source != target]
  cbin_dt[, cosine_bin_sim  := dot / (sqrt(rs_vec[source]) * sqrt(rs_vec[target]))]
  cbin_dt  <- cbin_dt[is.finite(cosine_bin_sim) & cosine_bin_sim > 0]
  cbin_dt[, cosine_bin_dist := 1 - cosine_bin_sim]
  cbin_dt[, c("dot", "cosine_bin_sim") := NULL]

  # Cosine ponderada
  message("   Calculando Cosine (ponderada por rca_t0)...")
  dot_wt   <- tcrossprod(M_wt)
  norm_wt  <- sqrt(Matrix::rowSums(M_wt^2))
  norm_vec <- as.numeric(norm_wt); names(norm_vec) <- occ_ids
  dotw_sum <- summary(dot_wt)
  cwt_dt <- data.table(
    source = occ_ids[dotw_sum$i],
    target = occ_ids[dotw_sum$j],
    dot    = as.numeric(dotw_sum$x)
  )[source != target]
  cwt_dt[, cosine_wt_sim  := dot / (norm_vec[source] * norm_vec[target])]
  cwt_dt  <- cwt_dt[is.finite(cosine_wt_sim) & cosine_wt_sim > 0]
  cwt_dt[, cosine_wt_dist := 1 - cosine_wt_sim]
  cwt_dt[, c("dot", "cosine_wt_sim") := NULL]

  # Merge distancias
  message("   Consolidando distancias...")
  setkey(j_dt, source, target)
  setkey(cbin_dt, source, target)
  setkey(cwt_dt, source, target)
  dist_dt <- merge(j_dt, cbin_dt, all = TRUE)
  dist_dt <- merge(dist_dt, cwt_dt, all = TRUE)
  dist_dt[is.na(jaccard_dist),    jaccard_dist    := 1]
  dist_dt[is.na(cosine_bin_dist), cosine_bin_dist := 1]
  dist_dt[is.na(cosine_wt_dist),  cosine_wt_dist  := 1]

  # Shortest-path edges
  message("   Preparando red ocupacional para shortest-path...")
  occ_edges_for_sp <- dist_dt[, .(source, target, sim = 1 - cosine_wt_dist)]
  occ_edges_for_sp <- occ_edges_for_sp[source != target]
  occ_edges_for_sp[, sim   := pmin(1, pmax(0, sim))]
  occ_edges_for_sp <- occ_edges_for_sp[sim >= 0.10]
  occ_edges_for_sp[, w_dist := pmax(0, 1 - sim)]
  occ_edges_for_sp[, sim := NULL]
  setkey(occ_edges_for_sp, source, target)
  occ_edges_for_sp <- occ_edges_for_sp[, .(w_dist = min(w_dist, na.rm = TRUE)),
                                        by = .(source, target)]
  save_rds(occ_edges_for_sp, "occ_edges_for_sp_2015.rds")

  rm(mat, sim_skill, g_skill, lou, mat_dt,
     occ_wide_bin, occ_mat_bin, occ_wide_wt, occ_mat_wt,
     M_bin, M_wt, inter_bin, dot_wt, rs_bin, norm_wt,
     inter_sum, dotw_sum, j_dt, cbin_dt, cwt_dt)
  gc()
}

# ==============================================================================
# 6. CONSTRUCCIÓN DEL PANEL (RISK SET & OUTCOMES MULTI-THRESHOLD)
# ==============================================================================
message("\n>>> 4. CONSTRUYENDO DIADAS (RISK SET BASELINE RCA > 1.0)...")

panel <- merge(rca_15, rca_24, by = c("soc_code", "skill"), all = TRUE)
panel[is.na(rca_t0), rca_t0 := 0]
panel[is.na(rca_t1), rca_t1 := 0]

sources_t0 <- panel[rca_t0 > 1.0, .(source = soc_code, skill)]

targets_risk <- panel[rca_t0 <= 1.0,
                      .(target       = soc_code,
                        skill,
                        rca_t0_target = rca_t0,
                        rca_t1_target = rca_t1)]

targets_risk[, diffusion     := fifelse(rca_t1_target >= 1.00, 1L, 0L)]
targets_risk[, diffusion_075 := fifelse(rca_t1_target >= 0.75, 1L, 0L)]
targets_risk[, diffusion_125 := fifelse(rca_t1_target >= 1.25, 1L, 0L)]
targets_risk[, diffusion_150 := fifelse(rca_t1_target >= 1.50, 1L, 0L)]

message("Risk Set Total (Pares Target-Skill): ",
        format(nrow(targets_risk), big.mark = ","))

all_dyads_list <- list()
skills_vec     <- unique(sources_t0$skill)
pb <- progress_bar$new(total = length(skills_vec))

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
    source = srcs,
    target = tgts_final$target,
    stringsAsFactors = FALSE
  ))
  pairs <- pairs[source != target]

  pairs <- merge(pairs,
                 tgts_final[, .(target,
                                diffusion,
                                diffusion_075,
                                diffusion_125,
                                diffusion_150,
                                rca_t0_target,
                                rca_t1_target)],
                 by = "target")
  pairs[, skill_name := sk]

  all_dyads_list[[sk]] <- pairs
}

all_events <- rbindlist(all_dyads_list)
rm(all_dyads_list, panel, sources_t0, targets_risk); gc()

# ==============================================================================
# 7. ENRIQUECIMIENTO (SALARIOS 2015 & DISTANCIAS)
# ==============================================================================
message("\n>>> 5. ENRIQUECIENDO (SALARIOS 2015 & DISTANCIAS)...")

get_wages <- function(file) {
  f <- file.path(data_external_path, file)
  if (!file.exists(f)) return(NULL)
  d  <- as.data.table(read_excel(f, sheet = 1))
  cc <- grep("OCC_CODE", names(d), value = TRUE)[1]
  cw <- grep("A_MEDIAN", names(d), value = TRUE)[1]
  if (is.na(cc) || is.na(cw)) return(NULL)
  d[, w := suppressWarnings(as.numeric(as.character(get(cw))))]
  d[, c := clean_id(get(cc))]
  return(d[!is.na(w), .(wage = mean(w), log_w = log(mean(w))), by = c])
}

w15 <- get_wages(file_bls_15)
if (is.null(w15)) stop("No se pudo cargar salarios 2015.")

s_wage <- copy(w15)
setnames(s_wage, c("c", "wage", "log_w"), c("source", "s_wage", "log_s_wage"))
all_events <- merge(all_events, s_wage, by = "source", all.x = TRUE)

t_wage <- copy(w15)
setnames(t_wage, c("c", "wage", "log_w"), c("target", "t_wage", "log_t_wage"))
all_events <- merge(all_events, t_wage, by = "target", all.x = TRUE)

all_events[, wage_gap      := log_t_wage - log_s_wage]

all_events[, wage_up   := pmax(0,  wage_gap)]
all_events[, wage_down := pmin(0,  wage_gap)]
all_events[, up_dummy  := fifelse(wage_gap > 0, 1L, 0L)]

all_events <- merge(all_events,
                    skill_classes[, .(skill, domain)],
                    by.x = "skill_name", by.y = "skill", all.x = TRUE)

if (!is.null(dist_dt)) {
  setkey(all_events, source, target)
  setkey(dist_dt, source, target)
  all_events <- merge(all_events, dist_dt, all.x = TRUE)
  all_events[is.na(cosine_wt_dist),  cosine_wt_dist  := 1]
} else {
  all_events[, cosine_wt_dist  := 1]
}

all_events[, structural_distance := cosine_wt_dist]

# ==============================================================================
# 8. DIAGNÓSTICO FINAL Y GUARDADO
# ==============================================================================
message("\n>>> 6. DIAGNÓSTICO FINAL <<<")

final_dt <- all_events[!is.na(domain)]
final_dt[, year_adoption := 2023]

save_path <- file.path(output_data_dir, "all_events_v13_multithr.rds")
saveRDS(final_dt, save_path)
message("\n>>> GUARDADO: ", save_path)

rm(list = setdiff(ls(), "final_dt")); gc()
