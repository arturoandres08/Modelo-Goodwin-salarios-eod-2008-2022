# codes/19c_svar_core_summary_table.R
# ==========================================================
# 19c) Tabla resumen SVAR núcleo
# - Lee outputs generados por 19 (y opcionalmente 19b)
# - Crea:
#   tables/svar_core/summary/svar_core_summary_main.csv
#   tables/svar_core/summary/svar_core_summary_with_lag_robustness.csv (si existe 19b)
# ==========================================================

source(here::here("codes","00_setup.R"))
message("\n== 19c) Tabla resumen SVAR núcleo ==\n")

# ---- dirs según tu setup ----
dir_core <- fs::path(CFG$dirs$out_tables, "svar_core")
dir_lag  <- fs::path(dir_core, "lag_robustness")
out_dir  <- fs::path(dir_core, "summary")

fs::dir_create(out_dir)

tags <- c("baseline_mean_winsor", "robust_mean_nowinsor", "robust_median")

# ---- helpers ----
read_diag_tag <- function(tg) {
  f <- fs::path(dir_core, paste0("var_diagnostics_", tg, ".csv"))
  stopifnot(file.exists(f))
  d <- readr::read_csv(f, show_col_types = FALSE) %>%
    dplyr::mutate(
      tag = tg,
      max_root_mod = as.numeric(max_root_mod),
      serial_p = as.numeric(serial_p),
      arch_p = as.numeric(arch_p),
      p_bic = as.integer(p_bic),
      p_aic = as.integer(p_aic),
      p_used = p_bic,
      criterion = "BIC/SC"
    ) %>%
    dplyr::select(tag, criterion, p_used, p_bic, p_aic, stable, max_root_mod, serial_p, arch_p)
  d
}

read_mult_tag <- function(tg) {
  f <- fs::path(dir_core, paste0("cum_multiplier_", tg, ".csv"))
  stopifnot(file.exists(f))
  m <- readr::read_csv(f, show_col_types = FALSE) %>%
    dplyr::mutate(tag = tg) %>%
    dplyr::filter(H %in% c(4, 8, 12)) %>%
    dplyr::select(tag, H, M, M_lower, M_upper) %>%
    tidyr::pivot_wider(
      names_from = H,
      values_from = c(M, M_lower, M_upper),
      names_glue = "{.value}_H{H}"
    )
  m
}

# ---- 1) Summary MAIN (tags de robustez salarial) ----
diag_main <- dplyr::bind_rows(purrr::map(tags, read_diag_tag))
mult_main <- dplyr::bind_rows(purrr::map(tags, read_mult_tag))

summary_main <- diag_main %>%
  dplyr::left_join(mult_main, by = "tag") %>%
  dplyr::arrange(factor(tag, levels = tags))

out_main <- fs::path(out_dir, "svar_core_summary_main.csv")
readr::write_csv(summary_main, out_main)
message("✔ Guardado: ", out_main)

# ---- 2) Opcional: incorporar robustez por rezagos (19b) ----
f_diag_lag <- fs::path(dir_lag, "diagnostics_p1_vs_p6.csv")
f_mult_lag <- fs::path(dir_lag, "cum_multiplier_p1_vs_p6.csv")

if (file.exists(f_diag_lag) && file.exists(f_mult_lag)) {
  
  diag_lag <- readr::read_csv(f_diag_lag, show_col_types = FALSE) %>%
    dplyr::mutate(
      # esto es baseline, pero con p fijo
      tag = paste0("baseline_mean_winsor_p", p),
      criterion = dplyr::case_when(
        p == 1 ~ "BIC/SC (fixed)",
        p == 6 ~ "AIC (fixed)",
        TRUE ~ "fixed"
      ),
      p_used = as.integer(p),
      p_bic = NA_integer_,
      p_aic = NA_integer_,
      max_root_mod = as.numeric(max_root_mod),
      serial_p = as.numeric(serial_p),
      arch_p = as.numeric(arch_p)
    ) %>%
    dplyr::select(tag, criterion, p_used, p_bic, p_aic, stable, max_root_mod, serial_p, arch_p)
  
  mult_lag <- readr::read_csv(f_mult_lag, show_col_types = FALSE) %>%
    dplyr::mutate(tag = paste0("baseline_mean_winsor_p", p)) %>%
    dplyr::filter(H %in% c(4, 8, 12)) %>%
    dplyr::select(tag, H, M, M_lower, M_upper) %>%
    tidyr::pivot_wider(
      names_from = H,
      values_from = c(M, M_lower, M_upper),
      names_glue = "{.value}_H{H}"
    )
  
  summary_lag <- diag_lag %>%
    dplyr::left_join(mult_lag, by = "tag") %>%
    dplyr::arrange(p_used)
  
  summary_all <- dplyr::bind_rows(summary_main, summary_lag)
  
  out_all <- fs::path(out_dir, "svar_core_summary_with_lag_robustness.csv")
  readr::write_csv(summary_all, out_all)
  message("✔ Guardado: ", out_all)
  
} else {
  message("ℹ No encontré archivos de 19b en: ", as.character(dir_lag),
          " (se mantiene solo el summary_main).")
}

# ---- print rápido ----
print(summary_main)