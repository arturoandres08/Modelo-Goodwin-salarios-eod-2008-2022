# codes/20d_svar_nocovid_lag_extension.R
# ==========================================================
# 20d) Robustez noCOVID con más rezagos
# - Estima SVAR baseline en submuestra noCOVID (2008Q1-2019Q4)
# - Compara p fijo = 4, 5 y 6
# - Mantiene la misma lógica del baseline central, pero sin COVID
# Outputs:
#   tables/svar_core/covid_nocovid_lags/*
#   figures/svar_core/covid_nocovid_lags/*
# ==========================================================

source(here::here("codes", "00_setup.R"))
message("\n== 20d) Robustez noCOVID con más rezagos (p=4,5,6) ==\n")

pkgs <- c("zoo", "vars", "tseries")
to_install <- pkgs[!vapply(pkgs, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)]
if (length(to_install) > 0) install.packages(to_install)

library(zoo)
library(vars)
library(tseries)

source(here::here("codes", "18_svar_core_functions.R"))

# ---------------------------
# Parámetros globales
# ---------------------------
P_GRID <- c(4L, 5L, 6L)
N_AHEAD <- 12
RUNS_BOOT <- 2000
CI_LEVEL <- 0.95
SEED0 <- 123
WAGE_COL <- "w_real_mean_winsor"
TAG_BASE <- "baseline_mean_winsor_nocovid"

# ---------------------------
# Output dirs
# ---------------------------
dir_tbl <- as.character(fs::path(CFG$dirs$out_tables, "svar_core", "covid_nocovid_lags"))
dir_fig <- as.character(fs::path(CFG$dirs$out_figures, "svar_core", "covid_nocovid_lags"))
fs::dir_create(dir_tbl)
fs::dir_create(dir_fig)

# ---------------------------
# Inputs
# ---------------------------
in_path <- fs::path(CFG$dirs$processed, "svar_core_inputs_2008Q1_2022Q4.rds")
stopifnot(file.exists(in_path))

df0 <- load_rds(in_path) %>%
  dplyr::mutate(q = zoo::as.yearqtr(paste0(year, " Q", trimestre), format = "%Y Q%q")) %>%
  dplyr::arrange(q)

q_end_nocovid <- zoo::as.yearqtr("2019 Q4", format = "%Y Q%q")
df_nocovid <- df0 %>% dplyr::filter(q <= q_end_nocovid)

# ---------------------------
# Helpers
# ---------------------------
fit_var_fixed_p <- function(Xmat, p_fixed) {
  eval(bquote(vars::VAR(Xmat, p = .(as.integer(p_fixed)), type = "const")))
}

run_one_nocovid <- function(df_in,
                            wage_col = WAGE_COL,
                            tag = TAG_BASE,
                            p_fixed,
                            n_ahead = N_AHEAD,
                            runs = RUNS_BOOT,
                            ci = CI_LEVEL,
                            seed = SEED0) {

  message("\n--- noCOVID | tag: ", tag, " | wage_col: ", wage_col, " | p_fixed=", p_fixed, " ---")

  X <- build_core(df_in, wage_col = wage_col, a_col = "a_prod")
  Xmat <- X %>% dplyr::select(ga, gw, v)

  ur <- dplyr::bind_rows(
    unit_root_table(X$ga, paste0("ga (", tag, ", noCOVID, p=", p_fixed, ")")),
    unit_root_table(X$gw, paste0("gw (", tag, ", noCOVID, p=", p_fixed, ")")),
    unit_root_table(X$v,  paste0("v (", tag, ", noCOVID, p=", p_fixed, ")"))
  ) %>%
    dplyr::mutate(sample = "noCOVID (2008Q1-2019Q4)", tag = tag, p_used = as.integer(p_fixed))

  var_fit <- fit_var_fixed_p(Xmat, p_fixed = p_fixed)
  diag <- diagnostics_var(var_fit) %>%
    dplyr::mutate(sample = "noCOVID (2008Q1-2019Q4)", tag = tag, p_used = as.integer(p_fixed))

  svar <- fit_svar_A0(var_fit)
  spec <- attr(svar, "svar_spec")
  if (!is.null(spec)) message("SVAR spec usado: B=", spec$B, " | method=", spec$method)

  set.seed(seed + as.integer(p_fixed))
  irf_v <- vars::irf(
    svar,
    impulse = "v",
    response = c("ga", "gw"),
    n.ahead = n_ahead,
    boot = TRUE,
    ci = ci,
    runs = runs
  )
  irf_v_ga <- tidy_irf(irf_v, "v", "ga") %>%
    dplyr::mutate(sample = "noCOVID (2008Q1-2019Q4)", impulse = "v", response = "ga", tag = tag, p_used = as.integer(p_fixed), ci = ci)
  irf_v_gw <- tidy_irf(irf_v, "v", "gw") %>%
    dplyr::mutate(sample = "noCOVID (2008Q1-2019Q4)", impulse = "v", response = "gw", tag = tag, p_used = as.integer(p_fixed), ci = ci)

  set.seed(seed + 100 + as.integer(p_fixed))
  irf_ga <- vars::irf(
    svar,
    impulse = "ga",
    response = c("gw"),
    n.ahead = n_ahead,
    boot = TRUE,
    ci = ci,
    runs = runs
  )
  irf_ga_gw <- tidy_irf(irf_ga, "ga", "gw") %>%
    dplyr::mutate(sample = "noCOVID (2008Q1-2019Q4)", impulse = "ga", response = "gw", tag = tag, p_used = as.integer(p_fixed), ci = ci)

  mult <- cum_multiplier(irf_v_ga, H_vec = c(4, 8, 12)) %>%
    dplyr::mutate(sample = "noCOVID (2008Q1-2019Q4)", tag = tag, p_used = as.integer(p_fixed))

  irf_all <- dplyr::bind_rows(irf_v_ga, irf_v_gw, irf_ga_gw)

  summary_irf <- irf_all %>%
    dplyr::group_by(sample, tag, p_used, impulse, response) %>%
    dplyr::summarise(
      sign_h0 = dplyr::first(sign(irf)),
      peak_h = h[which.max(abs(irf))][1],
      first_h_sig = {
        idx <- which(!(lower <= 0 & upper >= 0))
        if (length(idx) == 0) NA_integer_ else h[min(idx)]
      },
      last_h = max(h, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      peak_h = as.integer(peak_h),
      first_h_sig = as.integer(first_h_sig)
    )

  list(
    ur = ur,
    diag = diag,
    irf_v_ga = irf_v_ga,
    irf_v_gw = irf_v_gw,
    irf_ga_gw = irf_ga_gw,
    irf_all = irf_all,
    mult = mult,
    summary_irf = summary_irf,
    svar = svar
  )
}

plot_irf_compare_p <- function(df_irf, title, ylab) {
  ggplot2::ggplot(df_irf, ggplot2::aes(x = h, y = irf, group = factor(p_used), fill = factor(p_used), color = factor(p_used))) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.4) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), alpha = 0.12, colour = NA) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::labs(
      title = title,
      x = "Horizonte (trimestres)",
      y = ylab,
      color = "p",
      fill = "p"
    ) +
    ggplot2::theme_bw(base_size = 12)
}

# ---------------------------
# Correr grilla de rezagos
# ---------------------------
res_list <- lapply(P_GRID, function(pp) {
  run_one_nocovid(df_nocovid, p_fixed = pp)
})
names(res_list) <- paste0("p", P_GRID)

ur_all <- dplyr::bind_rows(lapply(res_list, `[[`, "ur"))
diag_all <- dplyr::bind_rows(lapply(res_list, `[[`, "diag"))
irf_v_ga_all <- dplyr::bind_rows(lapply(res_list, `[[`, "irf_v_ga"))
irf_v_gw_all <- dplyr::bind_rows(lapply(res_list, `[[`, "irf_v_gw"))
irf_ga_gw_all <- dplyr::bind_rows(lapply(res_list, `[[`, "irf_ga_gw"))
irf_all <- dplyr::bind_rows(lapply(res_list, `[[`, "irf_all"))
mult_all <- dplyr::bind_rows(lapply(res_list, `[[`, "mult"))
summary_irf_all <- dplyr::bind_rows(lapply(res_list, `[[`, "summary_irf"))

# Tabla simple de significancia por horizonte
sig_grid <- irf_all %>%
  dplyr::mutate(excludes_zero = !(lower <= 0 & upper >= 0)) %>%
  dplyr::select(sample, tag, p_used, impulse, response, h, irf, lower, upper, excludes_zero)

# ---------------------------
# Guardar tablas
# ---------------------------
readr::write_csv(ur_all, fs::path(dir_tbl, "unit_root_nocovid_lag_extension.csv"))
readr::write_csv(diag_all, fs::path(dir_tbl, "var_diagnostics_nocovid_lag_extension.csv"))
readr::write_csv(mult_all, fs::path(dir_tbl, "cum_multiplier_nocovid_lag_extension.csv"))
readr::write_csv(irf_v_ga_all, fs::path(dir_tbl, "irf_v_to_ga_nocovid_lag_extension.csv"))
readr::write_csv(irf_v_gw_all, fs::path(dir_tbl, "irf_v_to_gw_nocovid_lag_extension.csv"))
readr::write_csv(irf_ga_gw_all, fs::path(dir_tbl, "irf_ga_to_gw_nocovid_lag_extension.csv"))
readr::write_csv(summary_irf_all, fs::path(dir_tbl, "irf_summary_nocovid_lag_extension.csv"))
readr::write_csv(sig_grid, fs::path(dir_tbl, "irf_significance_nocovid_lag_extension.csv"))

# ---------------------------
# Figuras
# ---------------------------
p1 <- plot_irf_compare_p(
  irf_v_ga_all,
  "IRF noCOVID: shock empleo (v) -> productividad (ga) | comparación p = 4, 5, 6",
  "ga"
)
p2 <- plot_irf_compare_p(
  irf_v_gw_all,
  "IRF noCOVID: shock empleo (v) -> salarios (gw) | comparación p = 4, 5, 6",
  "gw"
)
p3 <- plot_irf_compare_p(
  irf_ga_gw_all,
  "IRF noCOVID: shock productividad (ga) -> salarios (gw) | comparación p = 4, 5, 6",
  "gw"
)

ggplot2::ggsave(filename = file.path(dir_fig, "irf_v_to_ga_nocovid_lag_extension.pdf"), plot = p1, width = 8, height = 5)
ggplot2::ggsave(filename = file.path(dir_fig, "irf_v_to_gw_nocovid_lag_extension.pdf"), plot = p2, width = 8, height = 5)
ggplot2::ggsave(filename = file.path(dir_fig, "irf_ga_to_gw_nocovid_lag_extension.pdf"), plot = p3, width = 8, height = 5)

message("\n✔ 20d listo: revisa tables/svar_core/covid_nocovid_lags y figures/svar_core/covid_nocovid_lags\n")
