# codes/22c_svar_trend_checks_p4.R
# ==========================================================
# 22c) Evidencia de tendencia determinística (SVAR core, p=4, 3 specs)
# - Regresión simple: y ~ t
# - Regresión con estacionalidad: y ~ t + dummies trimestrales
# Outputs:
#   tables/svar_core/p4_fixed/trend/trend_tests.csv
# ==========================================================

source(here::here("codes","00_setup.R"))
message("\n== 22c) Checks tendencia determinística (y ~ t) ==\n")

source(here::here("codes","18_svar_core_functions.R"))

dir_tbl <- as.character(fs::path(CFG$dirs$out_tables, "svar_core", "p4_fixed", "trend"))
fs::dir_create(dir_tbl)

in_path <- fs::path(CFG$dirs$processed, "svar_core_inputs_2008Q1_2022Q4.rds")
stopifnot(file.exists(in_path))

df0 <- load_rds(in_path) %>%
  dplyr::mutate(q = zoo::as.yearqtr(paste0(year, " Q", trimestre), format = "%Y Q%q")) %>%
  dplyr::arrange(q)

specs <- tibble::tibble(
  spec = c("Baseline (mean winsor)", "Robustez (mean no-winsor)", "Robustez (median)"),
  wage_col = c("w_real_mean_winsor", "w_real_mean_nowinsor", "w_real_median")
)

get_qtr <- function(q) factor(as.integer(format(q, "%q")), levels = 1:4, labels = c("Q1","Q2","Q3","Q4"))

trend_row <- function(y, t, qtr_f) {
  m1 <- stats::lm(y ~ t)
  m2 <- stats::lm(y ~ t + qtr_f)
  
  b1 <- summary(m1)$coef["t", ]
  b2 <- summary(m2)$coef["t", ]
  
  tibble::tibble(
    beta_t_simple = as.numeric(b1["Estimate"]),
    p_t_simple    = as.numeric(b1["Pr(>|t|)"]),
    r2_simple     = summary(m1)$r.squared,
    
    beta_t_seas = as.numeric(b2["Estimate"]),
    p_t_seas    = as.numeric(b2["Pr(>|t|)"]),
    r2_seas     = summary(m2)$r.squared
  )
}

rows <- list()

for (i in seq_len(nrow(specs))) {
  sp <- specs$spec[i]
  wcol <- specs$wage_col[i]
  
  X <- build_core(df0, wage_col = wcol, a_col = "a_prod") %>%
    dplyr::mutate(
      t = seq_len(dplyr::n()),
      qtr_f = get_qtr(q)
    )
  
  for (vn in c("ga","gw","v")) {
    tmp <- trend_row(X[[vn]], X$t, X$qtr_f)
    rows[[length(rows)+1]] <- tmp %>%
      dplyr::mutate(spec = sp, variable = vn, T = nrow(X)) %>%
      dplyr::select(spec, variable, T, dplyr::everything())
  }
}

out <- dplyr::bind_rows(rows) %>%
  dplyr::arrange(spec, factor(variable, levels = c("ga","gw","v")))

readr::write_csv(out, file.path(dir_tbl, "trend_tests.csv"))
message("✔ Guardado: ", file.path(dir_tbl, "trend_tests.csv"))
message("✔ 22c listo: tendencia determinística documentada (simple y + estacionalidad)")