# codes/22d_unitroot_extended_drift_trend_mu_tau_p4.R
# ==========================================================
# 22d) ADF/KPSS extendido (drift vs trend) y (mu vs tau)
# - Para 3 specs salariales (gw cambia) con p=4 fixed sample
# - Devuelve tabla comparativa (stats + CVs + decisiones)
# Outputs:
#   tables/svar_core/p4_fixed/unitroot_extended/unitroot_extended_p4_all_specs.csv
# ==========================================================

source(here::here("codes","00_setup.R"))
message("\n== 22d) Unit roots extendido: ADF drift/trend y KPSS mu/tau ==\n")

source(here::here("codes","18_svar_core_functions.R"))  # usa unit_root_table ya corregida

dir_tbl <- as.character(fs::path(CFG$dirs$out_tables, "svar_core", "p4_fixed", "unitroot_extended"))
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

# combinaciones pedidas (al menos drift y trend; mu y tau)
combos <- tidyr::expand_grid(
  adf_type = c("drift","trend"),
  kpss_type = c("mu","tau")
)

rows <- list()

for (i in seq_len(nrow(specs))) {
  sp <- specs$spec[i]
  wcol <- specs$wage_col[i]
  
  X <- build_core(df0, wage_col = wcol, a_col = "a_prod")
  
  for (vn in c("ga","gw","v")) {
    y <- X[[vn]]
    
    for (j in seq_len(nrow(combos))) {
      adf_t <- combos$adf_type[j]
      kpss_t <- combos$kpss_type[j]
      
      r <- unit_root_table(
        y, paste0(vn, " (", sp, ")"),
        adf_type = adf_t,
        adf_lag_select = "AIC",
        kpss_type = kpss_t,
        kpss_lags = "short"
      ) %>%
        dplyr::mutate(spec = sp, var_short = vn) %>%
        dplyr::select(spec, var_short, T,
                      ADF_type, ADF_stat, ADF_cv_10, ADF_cv_5, ADF_cv_1, ADF_reject_5pct,
                      KPSS_type, KPSS_stat, KPSS_cv_10, KPSS_cv_5, KPSS_cv_1, KPSS_reject_5pct)
      
      rows[[length(rows)+1]] <- r
    }
  }
}

out <- dplyr::bind_rows(rows) %>%
  dplyr::arrange(spec, factor(var_short, levels = c("ga","gw","v")), ADF_type, KPSS_type)

readr::write_csv(out, file.path(dir_tbl, "unitroot_extended_p4_all_specs.csv"))
message("✔ Guardado: ", file.path(dir_tbl, "unitroot_extended_p4_all_specs.csv"))
message("✔ 22d listo: cuadro comparativo drift/trend y mu/tau")