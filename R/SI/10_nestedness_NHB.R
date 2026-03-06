# =============================================================================
# 10_nestedness_NHB.R
# Nestedness computation using disparity-filter binarization (Serrano et al.)
#
# PURPOSE: Alternative nestedness contribution (c_s) computation that uses
# a disparity filter on the continuous importance matrix instead of the RCA
# binary threshold. This provides a robustness check for the archetype
# classification by testing whether c_s values are sensitive to the
# binarization method.
#
# RELATION TO PAPER: The main analysis uses RCA-based binarization
# (R/08_nestedness.R). This script tests sensitivity to that choice.
# Results are referenced in the Methods section discussion of nestedness
# construction robustness.
# =============================================================================

gc()

# 0) PACKAGES ------------------------------------------------------------------
pkgs <- c("data.table", "vegan", "future.apply", "progressr", "future")
to_install <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(to_install)) install.packages(to_install)

library(data.table)
library(vegan)
library(future.apply)
library(progressr)
library(future)

stopifnot(exists("dt"))
dt <- as.data.table(dt)

n_cores <- max(1L, parallel::detectCores() - 1L)
plan(multisession, workers = n_cores)

handlers(handler_progress(
  format = "[:bar] :percent :eta :message",
  clear = FALSE
))

# RNG robusto en paralelo
RNGkind("L'Ecuyer-CMRG")
set.seed(123)

message(">>> Sistema listo con ", n_cores, " núcleos.")


# 1) DISPARITY FILTER (por filas) + CONSTRUCCIÓN MATRIZ BINARIA -----------------
# Disparity filter estándar (Serrano et al.) aplicado por ocupación (fila) para
# seleccionar links significativos desde una matriz continua W.
disparity_filter_rows <- function(W, alpha = 0.05) {
  W <- as.matrix(W)
  nr <- nrow(W); nc <- ncol(W)
  B <- matrix(0L, nr, nc, dimnames = dimnames(W))
  rs <- rowSums(W)
  
  for (r in seq_len(nr)) {
    s <- rs[r]
    if (!is.finite(s) || s <= 0) next
    
    idx <- which(W[r, ] > 0)
    k <- length(idx)
    
    if (k == 0) next
    if (k == 1) {
      B[r, idx] <- 1L
      next
    }
    
    p <- W[r, idx] / s
    # p-value disparity: a_ij = 1 - (1 - p_ij)^(k-1)
    aij <- 1 - (1 - p)^(k - 1)
    
    B[r, idx[aij < alpha]] <- 1L
  }
  B
}

# Construcción de M binaria:
occ_col   <- "source"
skill_col <- "skill_name"

# Detectar columna continua si existe
value_col <- NULL
candidates <- c("onet_level", "level", "importance", "value", "score")
for (cc in candidates) if (cc %in% names(dt)) { value_col <- cc; break }

message(">>> Construyendo matriz M (binaria) ...")

if (!is.null(value_col)) {
  message(">>> Usando columna continua '", value_col, "' + disparity filter.")
  
  # Construir long con peso continuo
  dt_ws <- dt[, .(soc = get(occ_col), skill = get(skill_col), w = get(value_col))]
  dt_ws <- dt_ws[!is.na(w) & is.finite(w)]
  
  # si hay duplicados soc-skill, agrega (max es típico si w es "importance/level")
  dt_ws <- dt_ws[, .(w = max(w, na.rm = TRUE)), by = .(soc, skill)]
  
  # Wide continua
  W_wide <- dcast(dt_ws, soc ~ skill, value.var = "w", fill = 0)
  W <- as.matrix(W_wide[, -1])
  rownames(W) <- W_wide[[1]]
  
  # Disparity filter -> binaria backbone
  alpha_df <- 0.05  # ajusta si quieres más/menos backbone
  M <- disparity_filter_rows(W, alpha = alpha_df)
  
} else {
  message(">>> No hay columna continua. Usando binarización simple (presencia/ausencia).")
  
  occ_skill_pairs <- unique(dt[, .(soc = get(occ_col), skill = get(skill_col))])
  M_wide <- dcast(occ_skill_pairs, soc ~ skill, fun.aggregate = length, fill = 0)
  
  M <- as.matrix(M_wide[, -1])
  rownames(M) <- M_wide[[1]]
  M[M > 0] <- 1L
}

skill_names <- colnames(M)

if (nrow(M) < 3 || ncol(M) < 3) stop("Matriz demasiado pequeña para nestedness robusta.")

message(">>> M lista: ", nrow(M), " ocupaciones x ", ncol(M), " skills.")
message(">>> Densidad (aprox) = ", round(mean(M != 0), 4))


# 2) PRECOMPUTOS PARA ORDENAMIENTO ---------------------------------------------
# colSums NO cambia al permutar una columna (solo redistribuyes sus 1s entre filas)
col_sums <- colSums(M)
col_ord  <- order(col_sums, decreasing = TRUE)

# baseline rowSums
row_base <- rowSums(M)

# función: NODF con orden eficiente
nodf_fast <- function(M_bin, row_sums, col_ord) {
  row_ord <- order(row_sums, decreasing = TRUE)
  M_ord <- M_bin[row_ord, col_ord, drop = FALSE]
  vegan::nestednodf(M_ord, order = FALSE)$statistic["NODF"]
}

message(">>> Calculando NODF observado...")
N_obs <- nodf_fast(M, row_base, col_ord)
message(">>> NODF observado = ", round(N_obs, 4))


# 3) cs POR SKILL (OPTIMIZADO + PARALELO) --------------------------------------
n_sim_run <- 500L  # final (usa 500 para pruebas rápidas)
message(">>> Calculando cs por skill con n_sim_run = ", n_sim_run, " ...")

with_progress({
  p <- progressor(steps = length(skill_names))
  
  cs_results <- future_lapply(seq_along(skill_names), function(i) {
    p(message = sprintf("Skill %d/%d: %s", i, length(skill_names), skill_names[i]))
    
    old_vec <- M[, i]
    k <- sum(old_vec)
    if (k == 0L || k == length(old_vec)) return(NA_real_)
    
    null_dist <- numeric(n_sim_run)
    
    # loop interno (evita overhead de replicate)
    for (b in seq_len(n_sim_run)) {
      new_vec <- sample(old_vec, length(old_vec), replace = FALSE)
      
      # copiar matriz (simple y seguro); optimización incremental profunda sería reimplementar NODF
      M_null <- M
      M_null[, i] <- new_vec
      
      # rowSums actualizado sin recomputar todo
      row_sums_null <- row_base + (new_vec - old_vec)
      
      null_dist[b] <- nodf_fast(M_null, row_sums_null, col_ord)
    }
    
    mu <- mean(null_dist)
    sdv <- sd(null_dist)
    if (!is.finite(sdv) || sdv == 0) return(NA_real_)
    (N_obs - mu) / sdv
  }, future.seed = TRUE)
})

cs_vec <- unlist(cs_results, use.names = FALSE)

skill_stats <- data.table(
  skill_name = skill_names,
  cs = cs_vec
)

# 4) CONTINUA + BINARIA ---------------------------------------------------------
skill_stats[, nested_bin := as.integer(!is.na(cs) & cs > 0)]
skill_stats[, nestedness_cat := fifelse(is.na(cs), NA_character_,
                                        fifelse(cs > 0, "Nested", "Un-nested"))]

qs <- quantile(skill_stats$cs, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE, type = 7)
if (length(unique(qs)) < 4) {
  skill_stats[, nestedness_tercile := NA_character_]
} else {
  skill_stats[, nestedness_tercile := cut(
    cs, breaks = qs,
    labels = c("Low", "Mid", "High"),
    include.lowest = TRUE
  )]
}

# 5) MERGE ----------------------------------------------------------------------
dt <- merge(dt, skill_stats, by = "skill_name", all.x = TRUE)
gc()

message(">>> Listo: dt incluye cs (continua) + nested_bin (binaria) + terciles.")
saveRDS(dt, "dt_con_cs_nestedness.rds")
