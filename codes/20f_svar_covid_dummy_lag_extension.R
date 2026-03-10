# codes/20f_svar_covid_dummy_lag_extension.R
# ==========================================================
# 20f) Robustez COVID + rezagos: dummy COVID con p = 4 y 6
# - Muestra completa
# - Compara sin dummy vs con dummy COVID
# - Para p = 4 y p = 6
# Outputs:
#   tables/svar_core/covid_dummy_lags/*
#   figures/svar_core/covid_dummy_lags/*
# ==========================================================

source(here::here("codes", "00_setup.R"))

message("\n== 20f) Robustez COVID: dummy COVID con p = 4 y 6 ==\n")

pkgs <- c("zoo", "vars", "tseries", "dplyr", "readr", "ggplot2", "tidyr")
to_install <- pkgs[!vapply(pkgs, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)]
if (length(to_install) > 0) install.packages(to_install)

library(zoo)
library(vars)
library(tseries)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

source(here::here("codes", "18_svar_core_functions.R"))

# ---- parámetros ----
P_GRID    <- c(4L, 6L)
N_AHEAD   <- 12
RUNS_BOOT <- 2000
CI_LEVEL  <- 0.95
SEED0     <- 123
WAGE_COL  <- "w_real_mean_winsor"

# Ventana dummy COVID (editable, alineada con 20b)
Q0 <- zoo::as.yearqtr("2020 Q2", format = "%Y Q%q")
Q1 <- zoo::as.yearqtr("2021 Q1", format = "%Y Q%q")

# ---- outputs ----
dir_tbl <- as.character(fs::path(CFG$dirs$out_tables, "svar_core", "covid_dummy_lags"))
dir_fig <- as.character(fs::path(CFG$dirs$out_figures, "svar_core", "covid_dummy_lags"))
fs::dir_create(dir_tbl)
fs::dir_create(dir_fig)

# ---- inputs ----
in_path <- fs::path(CFG$dirs$processed, "svar_core_inputs_2008Q1_2022Q4.rds")
stopifnot(file.exists(in_path))

df0 <- load_rds(in_path) %>%
  dplyr::mutate(
    q = zoo::as.yearqtr(paste0(year, " Q", trimestre), format = "%Y Q%q")
  ) %>%
  dplyr::arrange(q)

# ---- helper: VAR p fijo con exógena literal ----
fit_var_fixed_p_exogen <- function(Xmat, p_fixed, exogen_mat = NULL) {
  if (is.null(exogen_mat)) {
    return(eval(bquote(vars::VAR(Xmat, p = .(as.integer(p_fixed)), type = "const"))))
  }
  eval(bquote(vars::VAR(
    Xmat,
    p = .(as.integer(p_fixed)),
    type = "const",
    exogen = .(exogen_mat)
  )))
}

model_label_fun <- function(p_fixed, include_dummy) {
  paste0("p=", p_fixed, " | ", ifelse(include_dummy, "con dummy COVID", "sin dummy"))
}

run_svar_dummy_lag <- function(df_in,
                               include_dummy = FALSE,
                               p_fixed = 4L,
                               wage_col = WAGE_COL,
                               n_ahead = N_AHEAD,
                               runs = RUNS_BOOT,
                               ci = CI_LEVEL,
                               seed = SEED0) {

  X <- build_core(df_in, wage_col = wage_col, a_col = "a_prod")
  Xmat <- X %>% dplyr::select(ga, gw, v)

  exo <- NULL
  if (include_dummy) {
    covid_dummy <- as.integer(X$q >= Q0 & X$q <= Q1)
    exo <- matrix(covid_dummy, ncol = 1)
    colnames(exo) <- "D_COVID"
  }

  model_tag   <- paste0("p", p_fixed, "_", ifelse(include_dummy, "with_dummy", "no_dummy"))
  model_label <- model_label_fun(p_fixed, include_dummy)

  var_fit <- fit_var_fixed_p_exogen(Xmat, p_fixed = p_fixed, exogen_mat = exo)

  diag_tbl <- diagnostics_var(var_fit) %>%
    dplyr::mutate(
      model = model_tag,
      model_label = model_label,
      dummy = include_dummy,
      p_used = as.integer(p_fixed),
      wage_col = wage_col
    )

  svar <- fit_svar_A0(var_fit)
  spec <- attr(svar, "svar_spec")
  specB <- if (!is.null(spec)) spec$B else NA_character_
  specM <- if (!is.null(spec)) spec$method else NA_character_

  diag_tbl <- diag_tbl %>%
    dplyr::mutate(
      svar_B = specB,
      svar_method = specM
    )

  set.seed(seed + as.integer(p_fixed) + ifelse(include_dummy, 100L, 0L))
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
    dplyr::mutate(
      impulse = "v",
      response = "ga",
      model = model_tag,
      model_label = model_label,
      dummy = include_dummy,
      p_used = as.integer(p_fixed),
      wage_col = wage_col,
      ci = ci
    )

  irf_v_gw <- tidy_irf(irf_v, "v", "gw") %>%
    dplyr::mutate(
      impulse = "v",
      response = "gw",
      model = model_tag,
      model_label = model_label,
      dummy = include_dummy,
      p_used = as.integer(p_fixed),
      wage_col = wage_col,
      ci = ci
    )

  set.seed(seed + 1000L + as.integer(p_fixed) + ifelse(include_dummy, 100L, 0L))
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
    dplyr::mutate(
      impulse = "ga",
      response = "gw",
      model = model_tag,
      model_label = model_label,
      dummy = include_dummy,
      p_used = as.integer(p_fixed),
      wage_col = wage_col,
      ci = ci
    )

  mult_v_ga <- cum_multiplier(irf_v_ga, H_vec = c(4, 8, 12)) %>%
    dplyr::transmute(
      H = H,
      multiplier = M,
      lower = M_lower,
      upper = M_upper,
      impulse = "v",
      response = "ga",
      model = model_tag,
      model_label = model_label,
      dummy = include_dummy,
      p_used = as.integer(p_fixed),
      wage_col = wage_col,
      ci = ci
    )

  list(
    diag = diag_tbl,
    irf  = dplyr::bind_rows(irf_v_ga, irf_v_gw, irf_ga_gw),
    mult = mult_v_ga
  )
}

# ---- correr grid ----
runs <- list()

for (pp in P_GRID) {
  message("--- Corriendo p = ", pp, " | sin dummy ---")
  runs[[paste0("p", pp, "_no_dummy")]] <- run_svar_dummy_lag(
    df_in = df0,
    include_dummy = FALSE,
    p_fixed = pp
  )

  message("--- Corriendo p = ", pp, " | con dummy COVID ---")
  runs[[paste0("p", pp, "_with_dummy")]] <- run_svar_dummy_lag(
    df_in = df0,
    include_dummy = TRUE,
    p_fixed = pp
  )
}

diag_all <- dplyr::bind_rows(lapply(runs, `[[`, "diag"))
irf_all  <- dplyr::bind_rows(lapply(runs, `[[`, "irf")) %>%
  dplyr::arrange(p_used, dummy, impulse, response, h)
mult_all <- dplyr::bind_rows(lapply(runs, `[[`, "mult")) %>%
  dplyr::arrange(p_used, dummy, H)

# ---- tablas resumen robustas ----
irf_significance <- irf_all %>%
  dplyr::mutate(
    excludes_zero = !(lower <= 0 & upper >= 0)
  ) %>%
  dplyr::select(
    model, model_label, p_used, dummy, wage_col,
    impulse, response, h, irf, lower, upper, excludes_zero
  )

irf_summary <- irf_all %>%
  dplyr::group_by(model, model_label, p_used, dummy, wage_col, impulse, response) %>%
  dplyr::summarise(
    sign_h0 = dplyr::first(sign(irf)),
    peak_h = as.integer(h[which.max(abs(irf))][1]),
    peak_irf = irf[which.max(abs(irf))][1],
    first_h_sig = {
      idx <- which(!(lower <= 0 & upper >= 0))
      if (length(idx) == 0) NA_integer_ else as.integer(h[min(idx)])
    },
    last_h = max(h, na.rm = TRUE),
    .groups = "drop"
  )

dummy_compare_summary <- irf_summary %>%
  dplyr::select(p_used, dummy, impulse, response, sign_h0, peak_h, peak_irf, first_h_sig) %>%
  dplyr::mutate(dummy_status = ifelse(dummy, "with_dummy", "no_dummy")) %>%
  dplyr::select(-dummy) %>%
  tidyr::pivot_wider(
    names_from = dummy_status,
    values_from = c(sign_h0, peak_h, peak_irf, first_h_sig)
  ) %>%
  dplyr::arrange(p_used, impulse, response)

mult_compare <- mult_all %>%
  dplyr::mutate(
    excludes_zero = !(lower <= 0 & upper >= 0),
    dummy_status = ifelse(dummy, "with_dummy", "no_dummy")
  ) %>%
  tidyr::pivot_wider(
    id_cols = c(p_used, H, impulse, response),
    names_from = dummy_status,
    values_from = c(multiplier, lower, upper, excludes_zero)
  ) %>%
  dplyr::arrange(p_used, H)

# ---- guardar tablas ----
readr::write_csv(diag_all, fs::path(dir_tbl, "diagnostics_covid_dummy_p4_p6.csv"))
readr::write_csv(irf_all, fs::path(dir_tbl, "irf_all_covid_dummy_p4_p6.csv"))
readr::write_csv(irf_significance, fs::path(dir_tbl, "irf_significance_covid_dummy_p4_p6.csv"))
readr::write_csv(irf_summary, fs::path(dir_tbl, "irf_summary_covid_dummy_p4_p6.csv"))
readr::write_csv(dummy_compare_summary, fs::path(dir_tbl, "irf_dummy_compare_summary_p4_p6.csv"))
readr::write_csv(mult_all, fs::path(dir_tbl, "cum_multiplier_covid_dummy_p4_p6.csv"))
readr::write_csv(mult_compare, fs::path(dir_tbl, "cum_multiplier_dummy_compare_p4_p6.csv"))

# ---- plots ----
plot_irf_facet <- function(df_irf, title, ylab) {
  ggplot2::ggplot(df_irf, ggplot2::aes(x = h, y = irf)) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.4) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), alpha = 0.20) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::facet_wrap(~model_label, ncol = 2) +
    ggplot2::labs(
      title = title,
      x = "Horizonte (trimestres)",
      y = ylab
    ) +
    ggplot2::theme_bw(base_size = 12)
}

p1 <- plot_irf_facet(
  irf_all %>% dplyr::filter(impulse == "v", response == "ga"),
  "IRF: shock empleo (v) -> productividad (ga) | p = 4 y 6, con/sin dummy COVID",
  "ga"
)

p2 <- plot_irf_facet(
  irf_all %>% dplyr::filter(impulse == "v", response == "gw"),
  "IRF: shock empleo (v) -> salarios (gw) | p = 4 y 6, con/sin dummy COVID",
  "gw"
)

p3 <- plot_irf_facet(
  irf_all %>% dplyr::filter(impulse == "ga", response == "gw"),
  "IRF: shock productividad (ga) -> salarios (gw) | p = 4 y 6, con/sin dummy COVID",
  "gw"
)

ggplot2::ggsave(
  filename = file.path(dir_fig, "irf_v_to_ga_covid_dummy_p4_p6.pdf"),
  plot = p1, width = 11, height = 6
)

ggplot2::ggsave(
  filename = file.path(dir_fig, "irf_v_to_gw_covid_dummy_p4_p6.pdf"),
  plot = p2, width = 11, height = 6
)

ggplot2::ggsave(
  filename = file.path(dir_fig, "irf_ga_to_gw_covid_dummy_p4_p6.pdf"),
  plot = p3, width = 11, height = 6
)

message("✔ 20f listo: ver tables/svar_core/covid_dummy_lags y figures/svar_core/covid_dummy_lags")
