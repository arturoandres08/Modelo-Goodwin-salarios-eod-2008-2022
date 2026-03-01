# codes/19_svar_core_run.R
# ==========================================================
# 19) Resultados SVAR núcleo (ga, gw, v) — Chile 2008–2022
# Baseline + robustez: mean winsor vs mean sin winsor vs mediana
# Outputs:
#   tables/svar_core/*   figures/svar_core/*
# (Usa CFG$dirs$out_tables y CFG$dirs$out_figures según 00_setup.R)
# ==========================================================

source(here::here("codes","00_setup.R"))
message("\n== 19) SVAR núcleo: estimación + robustez ==\n")

pkgs <- c("zoo", "tseries", "vars")
to_install <- pkgs[!vapply(pkgs, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)]
if (length(to_install) > 0) install.packages(to_install)

library(zoo)
library(tseries)
library(vars)

source(here::here("codes","18_svar_core_functions.R"))

# ---------------------------
# Salidas ordenadas (según tu setup)
# ---------------------------
dir_tbl <- as.character(fs::path(CFG$dirs$out_tables, "svar_core"))
dir_fig <- as.character(fs::path(CFG$dirs$out_figures, "svar_core"))

stopifnot(length(dir_tbl) == 1, nzchar(dir_tbl))
stopifnot(length(dir_fig) == 1, nzchar(dir_fig))

fs::dir_create(dir_tbl)
fs::dir_create(dir_fig)

# ---------------------------
# Cargar inputs del 17
# ---------------------------
in_path <- fs::path(CFG$dirs$processed, "svar_core_inputs_2008Q1_2022Q4.rds")
stopifnot(file.exists(in_path))
df <- load_rds(in_path)

df <- df %>%
  dplyr::mutate(q = zoo::as.yearqtr(paste0(year, " Q", trimestre), format = "%Y Q%q")) %>%
  dplyr::arrange(q)

# ---------------------------
# Runner SVAR
# ---------------------------
run_one <- function(wage_col, tag,
                    lag_max = 8, n_ahead = 12,
                    runs = 2000, ci = 0.95,
                    seed = 123) {
  
  message("\n--- RUN TAG: ", tag, " | wage_col: ", wage_col, " ---")
  
  X <- build_core(df, wage_col = wage_col, a_col = "a_prod")
  
  # Unit roots (avisos de tseries son normales)
  ur <- dplyr::bind_rows(
    unit_root_table(X$ga, paste0("ga (", tag, ")")),
    unit_root_table(X$gw, paste0("gw (", tag, ")")),
    unit_root_table(X$v,  paste0("v  (", tag, ")"))
  )
  
  # VAR select + diagnostics
  sel  <- select_lags_and_fit(X, lag_max = lag_max)
  diag <- diagnostics_var(sel$var_bic)
  
  p_bic_local <- sel$p_bic
  p_aic_local <- sel$p_aic
  message("p_bic=", p_bic_local, " | p_aic=", p_aic_local, " | stable=", diag$stable)
  
  lag_tbl <- tibble::tibble(
    criterion = names(sel$sel$selection),
    p = as.integer(sel$sel$selection),
    tag = tag
  )
  
  # SVAR (A0 teórico; fallback numérico B=I -> B diagonal libre)
  svar <- fit_svar_A0(sel$var_bic)
  spec <- attr(svar, "svar_spec")
  if (!is.null(spec)) message("SVAR spec usado: B=", spec$B, " | method=", spec$method)
  
  # IRFs (bootstrap reproducible)
  set.seed(seed)
  irf_v <- vars::irf(
    svar,
    impulse = "v",
    response = c("ga","gw"),
    n.ahead = n_ahead,
    boot = TRUE,
    ci = ci,
    runs = runs
  )
  irf_v_ga <- tidy_irf(irf_v, "v", "ga")
  irf_v_gw <- tidy_irf(irf_v, "v", "gw")
  
  set.seed(seed + 1)
  irf_ga <- vars::irf(
    svar,
    impulse = "ga",
    response = c("gw"),
    n.ahead = n_ahead,
    boot = TRUE,
    ci = ci,
    runs = runs
  )
  irf_ga_gw <- tidy_irf(irf_ga, "ga", "gw")
  
  mult <- cum_multiplier(irf_v_ga, H_vec = c(4,8,12)) %>%
    dplyr::mutate(tag = tag)
  
  # ---------------------------
  # Guardar tablas
  # ---------------------------
  readr::write_csv(dplyr::mutate(ur, tag = tag),
                   file.path(dir_tbl, paste0("unit_root_", tag, ".csv")))
  readr::write_csv(lag_tbl,
                   file.path(dir_tbl, paste0("lag_selection_", tag, ".csv")))
  
  diag_out <- dplyr::mutate(diag, tag = tag, p_bic = p_bic_local, p_aic = p_aic_local)
  readr::write_csv(diag_out,
                   file.path(dir_tbl, paste0("var_diagnostics_", tag, ".csv")))
  
  readr::write_csv(mult,
                   file.path(dir_tbl, paste0("cum_multiplier_", tag, ".csv")))
  
  # ---------------------------
  # Figuras
  # ---------------------------
  p1 <- plot_irf(irf_v_ga, paste0("IRF: shock empleo (v) → productividad (ga) [", tag, "]"), "ga")
  p2 <- plot_irf(irf_v_gw, paste0("IRF: shock empleo (v) → salarios (gw) [", tag, "]"), "gw")
  p3 <- plot_irf(irf_ga_gw, paste0("IRF: shock productividad (ga) → salarios (gw) [", tag, "]"), "gw")
  
  ggplot2::ggsave(filename = file.path(dir_fig, paste0("irf_v_to_ga_", tag, ".pdf")),
                  plot = p1, width = 7, height = 4.5)
  ggplot2::ggsave(filename = file.path(dir_fig, paste0("irf_v_to_gw_", tag, ".pdf")),
                  plot = p2, width = 7, height = 4.5)
  ggplot2::ggsave(filename = file.path(dir_fig, paste0("irf_ga_to_gw_", tag, ".pdf")),
                  plot = p3, width = 7, height = 4.5)
  
  invisible(list(X=X, ur=ur, sel=sel, diag=diag_out, mult=mult, svar=svar))
}

# --- Baseline y robustez ---
res_baseline <- run_one("w_real_mean_winsor",   "baseline_mean_winsor")
res_nowinsor <- run_one("w_real_mean_nowinsor", "robust_mean_nowinsor")
res_median   <- run_one("w_real_median",        "robust_median")

message("\n✔ 19 listo: revisa tables/svar_core y figures/svar_core\n")