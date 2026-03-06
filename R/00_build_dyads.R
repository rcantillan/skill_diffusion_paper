# ==============================================================================
# MASTER SCRIPT: SKILL DIFFUSION DATA CONSTRUCTION (V12 FINAL) — UPDATED (FIX SP)
# Logic: Strict Flow (Risk Set 2015->2024) + Baseline Status Gaps (t0)
# Adds: Multiple structural distance / relatedness measures computed at 2015:
#       - jaccard_dist (binary)
#       - cosine_bin_dist (binary)
#       - cosine_wt_dist (weighted by rca_t0)
#       - sp_dist (shortest-path distance on occ relatedness network, built from 2015)
# FIX: ensure non-negative weights and remove self-loops for shortest paths
# ==============================================================================

# --- 0. SETUP & LIBRERÍAS ---
gc()
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  data.table, readxl, progress, Matrix, lsa, cluster, igraph,
  stringr, dplyr, here
)

# --- 1. DEFINICIÓN DE RUTAS (AJUSTAR) ---
path_2015           <- "data/db_15_1"       # Baseline (t0)
path_2024           <- "data/db_29_2_text"  # Outcome (t1)
crosswalk_data_path <- "data/crosswalk/"
data_external_path  <- "data/"
output_data_dir     <- "datos_eventos_v12_FINAL"

if (!dir.exists(output_data_dir)) dir.create(output_data_dir, recursive = TRUE)

# Archivos auxiliares
file_cw_soc10_19 <- "2010_to_2019_Crosswalk.csv"
file_bls_15      <- "national_M2015_dl.xlsx"

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
  if (!file.exists(f)) { # Búsqueda flexible
    pat <- gsub(".txt", "", filename)
    f_alt <- list.files(folder, pattern = pat, full.names = TRUE)
    if(length(f_alt) > 0) f <- f_alt[1] else return(NULL)
  }

  dt <- fread(f, sep = "\t", quote = "", na.strings = c("NA", "n/a", "", "*"), showProgress=FALSE)
  setnames(dt, old=names(dt), new=make.names(names(dt))) # Scale ID -> Scale.ID

  if ("O.NET.SOC.Code" %in% names(dt)) dt[, soc_code := clean_id(O.NET.SOC.Code)]

  val_col <- grep("Data.?Value", names(dt), value=TRUE)[1]
  if(!is.na(val_col)) dt[, value := as.numeric(get(val_col))]

  return(dt)
}

calc_rca <- function(dt) {
  # Agregación por si hay duplicados tras el crosswalk
  agg <- dt[, .(v = mean(value, na.rm=TRUE)), by=.(soc_code, Element.Name)]

  occ_sum <- agg[, .(ot = sum(v)), by=soc_code]
  ski_sum <- agg[, .(st = sum(v)), by=Element.Name]
  gt      <- sum(agg$v)

  agg <- merge(agg, occ_sum, by="soc_code")
  agg <- merge(agg, ski_sum, by="Element.Name")
  agg[, rca := (v/ot)/(st/gt)]
  return(agg[, .(soc_code, skill = Element.Name, rca)])
}

save_rds <- function(x, name) {
  saveRDS(x, file.path(output_data_dir, name))
}

# ==============================================================================
# 3. PROCESAMIENTO BASELINE (2015 - t0)
# ==============================================================================
message("\n>>> 1. PROCESANDO 2015 (Baseline)...")

files <- c("Skills.txt", "Abilities.txt", "Knowledge.txt", "Work Activities.txt")
dt_15 <- rbindlist(lapply(files, function(x) load_onet(path_2015, x)), fill=TRUE)
dt_15 <- dt_15[Scale.ID == "IM" & !is.na(value)]

# --- Crosswalk SOC 2010 -> 2019 ---
cw_file <- file.path(crosswalk_data_path, file_cw_soc10_19)
if(file.exists(cw_file)) {
  cw <- fread(cw_file)
  cw_clean <- unique(cw[, .(
    soc10 = clean_id(`O*NET-SOC 2010 Code`),
    soc19 = clean_id(`O*NET-SOC 2019 Code`)
  )])
  dt_15 <- merge(dt_15, cw_clean, by.x="soc_code", by.y="soc10", all.x=FALSE, allow.cartesian=TRUE)
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

dt_24 <- rbindlist(lapply(files, function(x) load_onet(path_2024, x)), fill=TRUE)
dt_24 <- dt_24[Scale.ID == "IM" & !is.na(value)]
rca_24 <- calc_rca(dt_24)
setnames(rca_24, "rca", "rca_t1")
rm(dt_24); gc()

# ==============================================================================
# 5. CLASIFICACIÓN & DISTANCIAS (ESTRUCTURA t0 — 2015)
# ==============================================================================
message("\n>>> 3. CALCULANDO ESTRUCTURA DE RED Y DISTANCIAS (2015)...")

mat_dt <- rca_15[rca_t0 > 1]
skill_classes <- data.table(skill = unique(rca_15$skill), domain = "Unknown")

dist_dt <- NULL
occ_edges_for_sp <- NULL

if(nrow(mat_dt) > 0) {

  # --- A. Clusters de Skills (Louvain 2015) ---
  mat_wide <- dcast(mat_dt, skill ~ soc_code, value.var="rca_t0", fun.aggregate=length)
  mat <- as.matrix(mat_wide[, -1]); rownames(mat) <- mat_wide$skill

  sim_skill <- lsa::cosine(t(mat)); sim_skill[is.nan(sim_skill)] <- 0; diag(sim_skill) <- 0
  g_skill <- graph_from_adjacency_matrix(sim_skill, mode="undirected", weighted=TRUE)
  g_skill <- delete_edges(g_skill, E(g_skill)[weight < 0.1])
  lou <- cluster_louvain(g_skill)

  skill_classes <- data.table(skill = lou$names, cluster = paste0("C", lou$membership))
  skill_classes[, domain := fifelse(cluster == "C1", "Cognitive", "Physical")]

  # --- B. Matrices ocupación x skill (2015) ---
  message("   Construyendo matrices ocupación x skill (binaria y ponderada)...")

  occ_wide_bin <- dcast(mat_dt, soc_code ~ skill, fun.aggregate=length, value.var="rca_t0")
  occ_ids <- occ_wide_bin$soc_code
  occ_mat_bin <- as.matrix(occ_wide_bin[, -1])
  occ_mat_bin <- (occ_mat_bin > 0) * 1L

  occ_wide_wt <- dcast(mat_dt, soc_code ~ skill, fun.aggregate=mean, value.var="rca_t0")
  occ_mat_wt <- as.matrix(occ_wide_wt[, -1])
  occ_mat_wt[is.na(occ_mat_wt)] <- 0

  M_bin <- Matrix::Matrix(occ_mat_bin, sparse = TRUE)
  M_wt  <- Matrix::Matrix(occ_mat_wt,  sparse = TRUE)

  # ---------------------------------------------------------------------------
  # Distance 1: JACCARD (binary) — sparse long
  # ---------------------------------------------------------------------------
  message("   Calculando Jaccard (binaria) de forma sparse...")

  inter_bin <- tcrossprod(M_bin)
  rs_bin <- Matrix::rowSums(M_bin)

  inter_sum <- summary(inter_bin)
  j_dt <- data.table(
    source = occ_ids[inter_sum$i],
    target = occ_ids[inter_sum$j],
    inter  = as.numeric(inter_sum$x)
  )[source != target]

  rs_vec <- as.numeric(rs_bin); names(rs_vec) <- occ_ids
  j_dt[, union := rs_vec[source] + rs_vec[target] - inter]
  j_dt[, jaccard_sim := fifelse(union > 0, inter / union, 0)]
  j_dt <- j_dt[is.finite(jaccard_sim) & jaccard_sim > 0]
  j_dt[, jaccard_dist := 1 - jaccard_sim]
  j_dt[, c("inter","union","jaccard_sim") := NULL]

  # ---------------------------------------------------------------------------
  # Distance 2: COSINE (binary) — uses same intersections
  # ---------------------------------------------------------------------------
  message("   Calculando Cosine (binaria) de forma sparse...")

  cbin_dt <- data.table(
    source = occ_ids[inter_sum$i],
    target = occ_ids[inter_sum$j],
    dot    = as.numeric(inter_sum$x)
  )[source != target]

  cbin_dt[, cosine_bin_sim := dot / (sqrt(rs_vec[source]) * sqrt(rs_vec[target]))]
  cbin_dt <- cbin_dt[is.finite(cosine_bin_sim) & cosine_bin_sim > 0]
  cbin_dt[, cosine_bin_dist := 1 - cosine_bin_sim]
  cbin_dt[, c("dot","cosine_bin_sim") := NULL]

  # ---------------------------------------------------------------------------
  # Distance 3: COSINE (weighted) — sparse long
  # ---------------------------------------------------------------------------
  message("   Calculando Cosine (ponderada por rca_t0) de forma sparse...")

  dot_wt <- tcrossprod(M_wt)
  norm_wt <- sqrt(Matrix::rowSums(M_wt^2))
  norm_vec <- as.numeric(norm_wt); names(norm_vec) <- occ_ids

  dotw_sum <- summary(dot_wt)
  cwt_dt <- data.table(
    source = occ_ids[dotw_sum$i],
    target = occ_ids[dotw_sum$j],
    dot    = as.numeric(dotw_sum$x)
  )[source != target]

  cwt_dt[, cosine_wt_sim := dot / (norm_vec[source] * norm_vec[target])]
  cwt_dt <- cwt_dt[is.finite(cosine_wt_sim) & cosine_wt_sim > 0]
  cwt_dt[, cosine_wt_dist := 1 - cosine_wt_sim]
  cwt_dt[, c("dot","cosine_wt_sim") := NULL]

  # Merge all distances
  message("   Consolidando distancias en dist_dt...")

  setkey(j_dt, source, target)
  setkey(cbin_dt, source, target)
  setkey(cwt_dt, source, target)

  dist_dt <- merge(j_dt, cbin_dt, all = TRUE)
  dist_dt <- merge(dist_dt, cwt_dt,  all = TRUE)

  dist_dt[is.na(jaccard_dist),    jaccard_dist := 1]
  dist_dt[is.na(cosine_bin_dist), cosine_bin_dist := 1]
  dist_dt[is.na(cosine_wt_dist),  cosine_wt_dist := 1]

  # ---------------------------------------------------------------------------
  # Shortest-path network edges (2015) — FIXED to avoid negative weights & loops
  # ---------------------------------------------------------------------------
  message("   Preparando red ocupacional (edges) para shortest-path [FIX]...")

  thr_sim <- 0.10

  occ_edges_for_sp <- dist_dt[, .(
    source, target,
    sim = 1 - cosine_wt_dist
  )]

  # Remove self-loops explicitly
  occ_edges_for_sp <- occ_edges_for_sp[source != target]

  # Clamp similarity to [0,1] to avoid numeric issues
  occ_edges_for_sp[, sim := pmin(1, pmax(0, sim))]

  # Keep sufficiently strong edges
  occ_edges_for_sp <- occ_edges_for_sp[sim >= thr_sim]

  # Convert to non-negative distances
  occ_edges_for_sp[, w_dist := pmax(0, 1 - sim)]
  occ_edges_for_sp[, sim := NULL]

  # Collapse potential multi-edges (keep minimal cost per pair)
  setkey(occ_edges_for_sp, source, target)
  occ_edges_for_sp <- occ_edges_for_sp[, .(w_dist = min(w_dist, na.rm = TRUE)), by = .(source, target)]

  save_rds(occ_edges_for_sp, "occ_edges_for_sp_2015.rds")

  rm(mat, sim_skill, g_skill, lou, mat_dt,
     occ_wide_bin, occ_mat_bin, occ_wide_wt, occ_mat_wt,
     M_bin, M_wt, inter_bin, dot_wt, rs_bin, norm_wt,
     inter_sum, dotw_sum, j_dt, cbin_dt, cwt_dt)
  gc()
}

# ==============================================================================
# 6. CONSTRUCCIÓN DEL PANEL (RISK SET & FLOW)
# ==============================================================================
message("\n>>> 4. CONSTRUYENDO DIADAS (LÓGICA DE FLUJO)...")

panel <- merge(rca_15, rca_24, by=c("soc_code", "skill"), all=TRUE)
panel[is.na(rca_t0), rca_t0 := 0]
panel[is.na(rca_t1), rca_t1 := 0]

sources_t0 <- panel[rca_t0 > 1.0, .(source = soc_code, skill)]

targets_risk <- panel[rca_t0 <= 1.0, .(target = soc_code, skill, rca_t1)]
targets_risk[, diffusion := fifelse(rca_t1 > 1.0, 1L, 0L)]

message("   Risk Set Total (Pares Target-Skill): ", format(nrow(targets_risk), big.mark=","))

all_dyads_list <- list()
skills_vec <- unique(sources_t0$skill)
pb <- progress_bar$new(total = length(skills_vec))

for(sk in skills_vec) {
  pb$tick()
  srcs <- sources_t0[skill == sk]$source
  tgts <- targets_risk[skill == sk]

  if(length(srcs) == 0 || nrow(tgts) == 0) next

  pos <- tgts[diffusion == 1]
  neg <- tgts[diffusion == 0]
  if(nrow(neg) > 2000) neg <- neg[sample(.N, 2000)]
  tgts_final <- rbind(pos, neg)

  pairs <- as.data.table(expand.grid(source = srcs, target = tgts_final$target, stringsAsFactors = FALSE))
  pairs <- pairs[source != target]

  pairs <- merge(pairs, tgts_final[, .(target, diffusion)], by="target")
  pairs[, skill_name := sk]

  all_dyads_list[[sk]] <- pairs
}

all_events <- rbindlist(all_dyads_list)
rm(all_dyads_list, panel, sources_t0, targets_risk); gc()
message("\nBase de Eventos: ", format(nrow(all_events), big.mark=","))

# ==============================================================================
# 7. ENRIQUECIMIENTO Y STATUS GAPS (ATC LOG-LOGIC) - ACTUALIZADO V12.1
# ==============================================================================
message("\n>>> 5. ENRIQUECIENDO (SALARIOS 2015 & MULTI-GAPS)...")

get_wages <- function(file) {
  f <- file.path(data_external_path, file)
  if(!file.exists(f)) return(NULL)
  d <- as.data.table(read_excel(f, sheet=1))

  cc <- grep("OCC_CODE", names(d), value=TRUE)[1]
  cw <- grep("A_MEDIAN", names(d), value=TRUE)[1]
  if(is.na(cc) || is.na(cw)) return(NULL)

  d[, w := suppressWarnings(as.numeric(as.character(get(cw))))]
  d[, c := clean_id(get(cc))]

  return(d[!is.na(w), .(wage = mean(w), log_w = log(mean(w))), by=c])
}

w15 <- get_wages(file_bls_15)
if(is.null(w15)) stop("No se pudo cargar salarios 2015. Revisa la ruta o el nombre del archivo.")

s_wage <- copy(w15)
setnames(s_wage, c("c", "wage", "log_w"), c("source", "s_wage", "log_s_wage"))
all_events <- merge(all_events, s_wage, by="source", all.x=TRUE)

t_wage <- copy(w15)
setnames(t_wage, c("c", "wage", "log_w"), c("target", "t_wage", "log_t_wage"))
all_events <- merge(all_events, t_wage, by="target", all.x=TRUE)

# 7.1 Gaps
all_events[, wage_diff_abs := (t_wage - s_wage)]
all_events[, wage_diff_rel := (t_wage - s_wage) / s_wage]

all_events[, wage_gap := log_t_wage - log_s_wage]
all_events[, up_gap   := pmax(0, wage_gap)]
all_events[, down_gap := pmax(0, -wage_gap)]

all_events[, up_gap_raw   := pmax(0, wage_diff_abs)]
all_events[, down_gap_raw := pmax(0, -wage_diff_abs)]

# 7.2 Metadata (domain + distances 2015)
all_events <- merge(all_events, skill_classes[, .(skill, domain)],
                    by.x="skill_name", by.y="skill", all.x=TRUE)

if(!is.null(dist_dt)) {
  message("   Pegando distancias estructurales (2015) [jaccard, cosine_bin, cosine_wt]...")
  setkey(all_events, source, target)
  setkey(dist_dt, source, target)
  all_events <- merge(all_events, dist_dt, all.x=TRUE)

  all_events[is.na(jaccard_dist),    jaccard_dist := 1]
  all_events[is.na(cosine_bin_dist), cosine_bin_dist := 1]
  all_events[is.na(cosine_wt_dist),  cosine_wt_dist := 1]
} else {
  all_events[, jaccard_dist := 1]
  all_events[, cosine_bin_dist := 1]
  all_events[, cosine_wt_dist := 1]
}

# Shortest-path distance (2015 network) — FIXED
if (!is.null(occ_edges_for_sp) && nrow(occ_edges_for_sp) > 0) {
  message("   Calculando shortest-path distance en red ocupacional (2015)...")

  g_occ <- igraph::graph_from_data_frame(
    occ_edges_for_sp[, .(source, target, w_dist)],
    directed = FALSE
  )

  nodes <- igraph::V(g_occ)$name
  dmat <- igraph::distances(g_occ, v = nodes, to = nodes, weights = igraph::E(g_occ)$w_dist)

  pairs_need <- unique(all_events[, .(source, target)])
  pairs_need <- pairs_need[source != target]

  i <- match(pairs_need$source, nodes)
  j <- match(pairs_need$target, nodes)

  sp <- rep(NA_real_, nrow(pairs_need))
  ok <- !is.na(i) & !is.na(j)
  sp[ok] <- dmat[cbind(i[ok], j[ok])]

  pairs_need[, sp_dist := sp]

  max_finite <- suppressWarnings(max(pairs_need$sp_dist[is.finite(pairs_need$sp_dist)], na.rm = TRUE))
  if (!is.finite(max_finite)) max_finite <- 1
  pairs_need[!is.finite(sp_dist) | is.na(sp_dist), sp_dist := max_finite + 1]

  setkey(pairs_need, source, target)
  setkey(all_events, source, target)
  all_events <- merge(all_events, pairs_need, all.x = TRUE)

} else {
  message("   NOTE: occ_edges_for_sp not available; skipping sp_dist.")
  all_events[, sp_dist := NA_real_]
}

# Default main distance for modelling (choose one)
all_events[, structural_distance := cosine_wt_dist]

message(">>> Verificando Direccionalidad ATC:")
print(all_events[, .(
  Min_LogGap = min(wage_gap, na.rm=TRUE),
  Max_LogGap = max(wage_gap, na.rm=TRUE),
  Mean_UpGap = mean(up_gap, na.rm=TRUE),
  Mean_DownGap = mean(down_gap, na.rm=TRUE)
)])

message("Enriquecimiento completado.")

# ==============================================================================
# 8. DIAGNÓSTICO FINAL Y GUARDADO
# ==============================================================================
message("\n>>> 6. DIAGNÓSTICO FINAL <<<")

final_dt <- all_events[!is.na(wage_diff_rel) & !is.na(domain)]
final_dt[, year_adoption := 2023]

n_rows <- nrow(final_dt)
rate <- mean(final_dt$diffusion)
n_src <- uniqueN(final_dt$source)
n_tgt <- uniqueN(final_dt$target)

message("Observaciones Finales: ", format(n_rows, big.mark=","))
message("Tasa de Difusión (Flow): ", round(rate*100, 2), "%")
message("Ocupaciones Fuente: ", n_src)
message("Ocupaciones Destino: ", n_tgt)

gap_stats <- final_dt[, .(
  Upward_Pct = mean(wage_diff_rel > 0, na.rm=TRUE),
  Downward_Pct = mean(wage_diff_rel < 0, na.rm=TRUE)
)]
print(gap_stats)

# Guardar (descomentá si quieres)
#save_path <- file.path(output_data_dir, "all_events_final_enriched_REAL.rds")
#saveRDS(final_dt, save_path)
# message("\n>>> GUARDADO EXITOSO: ", save_path)

rm(list=setdiff(ls(), "final_dt")); gc()
glimpse(final_dt)