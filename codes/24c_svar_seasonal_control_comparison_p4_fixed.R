# codes/24c_svar_seasonal_control_comparison_p4_fixed.R
# ==========================================================
# 24c) Robustez de estacionalidad del SVAR núcleo (ga, gw, v)
# - Compara modelo SIN control estacional vs CON control estacional
# - p fijo = 4 (consistente con 19e)
# - Especificación salarial baseline_mean_winsor
# Outputs:
#   tables/svar_core/p4_fixed/seasonality_compare/*
#   figures/svar_core/p4_fixed/seasonality_compare/*
# ==========================================================

source(here::here("codes", "00_setup.R"))
message("\n== 24c) Robustez de estacionalidad: sin control vs con control estacional ==\n")

pkgs <- c("zoo", "vars")
to_install <- pkgs[!vapply(pkgs, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)]
if (length(to_install) > 0) install.packages(to_install)

library(zoo)
library(vars)

source(here::here("codes", "18_svar_core_functions.R"))

# ---------------------------
# Parámetros globales
# ---------------------------
P_FIXED   <- 4L
N_AHEAD   <- 12L
RUNS_BOOT <- 2000L
CI_LEVEL  <- 0.95
SEED0     <- 123
BASELINE_TAG <- "baseline_mean_winsor"
WAGE_COL  <- "w_real_mean_winsor"

# ---------------------------
# Output dirs
# ---------------------------
dir_tbl <- as.character(fs::path(CFG$dirs$out_tables, "svar_core", "p4_fixed", "seasonality_compare"))
dir_fig <- as.character(fs::path(CFG$dirs$out_figures, "svar_core", "p4_fixed", "seasonality_compare"))
fs::dir_create(dir_tbl)
fs::dir_create(dir_fig)

# ---------------------------
# Inputs del 17
# ---------------------------
in_path <- fs::path(CFG$dirs$processed, "svar_core_inputs_2008Q1_2022Q4.rds")
stopifnot(file.exists(in_path))

df <- load_rds(in_path) %>%
  dplyr::mutate(
    q = zoo::as.yearqtr(paste0(year, " Q", trimestre), format = "%Y Q%q")
  ) %>%
  dplyr::arrange(q)

# ---------------------------
# Helpers
# ---------------------------
fit_fixed_p <- function(Xmat, p_fixed, exogen = NULL) {
  if (is.null(exogen)) {
    eval(bquote(vars::VAR(Xmat, p = .(as.integer(p_fixed)), type = "const")))
  } else {
    eval(bquote(vars::VAR(Xmat, p = .(as.integer(p_fixed)), type = "const", exogen = .(exogen))))
  }
}

make_seasonal_exogen <- function(q_vec) {
  qtr_factor <- factor(cycle(q_vec), levels = 1:4, labels = c("Q1", "Q2", "Q3", "Q4"))
  exo <- stats::model.matrix(~ qtr_factor)[, -1, drop = FALSE]  # Q2-Q4, Q1 base
  colnames(exo) <- c("dum_Q2", "dum_Q3", "dum_Q4")
  exo
}

exclude_zero <- function(lower, upper) {
  dplyr::if_else(lower > 0 | upper < 0, TRUE, FALSE, missing = FALSE)
}

first_sig_h <- function(h, sig) {
  out <- h[which(sig)]
  if (length(out) == 0) return(NA_integer_)
  as.integer(out[1])
}

peak_abs_h <- function(h, irf) {
  if (length(irf) == 0 || all(is.na(irf))) return(NA_integer_)
  as.integer(h[which.max(abs(irf))[1]])
}

safe_sign <- function(x) {
  dplyr::case_when(
    is.na(x) ~ NA_character_,
    x > 0 ~ "positivo",
    x < 0 ~ "negativo",
    TRUE ~ "cero"
  )
}

summarise_irf <- function(tbl) {
  tbl %>%
    dplyr::group_by(model, impulse, response) %>%
    dplyr::summarise(
      sign_h0 = safe_sign(irf[h == 0][1]),
      first_sig_h = first_sig_h(h, exclude_zero),
      any_sig = any(exclude_zero, na.rm = TRUE),
      peak_abs_h = peak_abs_h(h, irf),
      peak_sign = safe_sign(irf[which.max(abs(irf))[1]]),
      .groups = "drop"
    )
}

plot_irf_compare <- function(tbl, title_txt, ylab_txt) {
  ggplot2::ggplot(tbl, ggplot2::aes(x = h, y = irf, color = model, fill = model)) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.4) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), alpha = 0.15, color = NA) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::scale_x_continuous(breaks = 0:max(tbl$h, na.rm = TRUE)) +
    ggplot2::labs(title = title_txt, x = "Horizonte", y = ylab_txt, color = NULL, fill = NULL) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "bottom")
}

run_one_seasonality <- function(seasonal = FALSE,
                                wage_col = WAGE_COL,
                                tag = BASELINE_TAG,
                                p_fixed = P_FIXED,
                                n_ahead = N_AHEAD,
                                runs = RUNS_BOOT,
                                ci = CI_LEVEL,
                                seed = SEED0) {

  model_tag <- if (seasonal) "with_seasonal_control" else "no_seasonal_control"
  message("\n--- MODEL: ", model_tag, " | wage_col: ", wage_col, " | p_fixed=", p_fixed, " ---")

  X <- build_core(df, wage_col = wage_col, a_col = "a_prod")
  Xmat <- X %>% dplyr::select(ga, gw, v)
  exo  <- if (seasonal) make_seasonal_exogen(X$q) else NULL

  ur <- dplyr::bind_rows(
    unit_root_table(X$ga, paste0("ga (", model_tag, ", p=", p_fixed, ")")),
    unit_root_table(X$gw, paste0("gw (", model_tag, ", p=", p_fixed, ")")),
    unit_root_table(X$v,  paste0("v (",  model_tag, ", p=", p_fixed, ")"))
  ) %>%
    dplyr::mutate(model = model_tag, seasonal_control = seasonal, tag = tag, p_used = as.integer(p_fixed))

  var_fit <- fit_fixed_p(Xmat, p_fixed = p_fixed, exogen = exo)
  diag <- diagnostics_var(var_fit) %>%
    dplyr::mutate(model = model_tag, seasonal_control = seasonal, tag = tag, p_used = as.integer(p_fixed))

  svar <- fit_svar_A0(var_fit)
  spec <- attr(svar, "svar_spec")
  if (!is.null(spec)) message("SVAR spec usado: B=", spec$B, " | method=", spec$method)

  set.seed(seed)
  irf_v <- vars::irf(
    svar,
    impulse = "v", response = c("ga", "gw"),
    n.ahead = n_ahead, boot = TRUE, ci = ci, runs = runs
  )
  irf_v_ga <- tidy_irf(irf_v, "v", "ga") %>%
    dplyr::mutate(
      impulse = "v",
      response = "ga",
      model = model_tag,
      seasonal_control = seasonal,
      tag = tag,
      p_used = as.integer(p_fixed),
      ci = ci
    )
  irf_v_gw <- tidy_irf(irf_v, "v", "gw") %>%
    dplyr::mutate(
      impulse = "v",
      response = "gw",
      model = model_tag,
      seasonal_control = seasonal,
      tag = tag,
      p_used = as.integer(p_fixed),
      ci = ci
    )

  set.seed(seed + 1)
  irf_ga <- vars::irf(
    svar,
    impulse = "ga", response = c("gw"),
    n.ahead = n_ahead, boot = TRUE, ci = ci, runs = runs
  )
  irf_ga_gw <- tidy_irf(irf_ga, "ga", "gw") %>%
    dplyr::mutate(
      impulse = "ga",
      response = "gw",
      model = model_tag,
      seasonal_control = seasonal,
      tag = tag,
      p_used = as.integer(p_fixed),
      ci = ci
    )

  mult_ga <- cum_multiplier(irf_v_ga, H_vec = c(4, 8, 12)) %>%
    dplyr::transmute(
      H = H,
      multiplier = M,
      lower = M_lower,
      upper = M_upper,
      model = model_tag,
      seasonal_control = seasonal,
      impulse = "v",
      response = "ga",
      tag = tag,
      p_used = as.integer(p_fixed),
      ci = ci
    )

  # Guardar outputs individuales
  readr::write_csv(ur, file.path(dir_tbl, paste0("unit_root_", model_tag, "_", tag, "_p", p_fixed, ".csv")))
  readr::write_csv(diag, file.path(dir_tbl, paste0("var_diagnostics_", model_tag, "_", tag, "_p", p_fixed, ".csv")))
  readr::write_csv(mult_ga, file.path(dir_tbl, paste0("cum_multiplier_v_to_ga_", model_tag, "_", tag, "_p", p_fixed, ".csv")))
  readr::write_csv(irf_v_ga, file.path(dir_tbl, paste0("irf_v_to_ga_", model_tag, "_", tag, "_p", p_fixed, ".csv")))
  readr::write_csv(irf_v_gw, file.path(dir_tbl, paste0("irf_v_to_gw_", model_tag, "_", tag, "_p", p_fixed, ".csv")))
  readr::write_csv(irf_ga_gw, file.path(dir_tbl, paste0("irf_ga_to_gw_", model_tag, "_", tag, "_p", p_fixed, ".csv")))

  invisible(list(
    model = model_tag,
    seasonal_control = seasonal,
    ur = ur,
    diag = diag,
    irf_v_ga = irf_v_ga,
    irf_v_gw = irf_v_gw,
    irf_ga_gw = irf_ga_gw,
    mult_ga = mult_ga,
    svar = svar
  ))
}

# ---------------------------
# Correr ambos modelos
# ---------------------------
res_no  <- run_one_seasonality(seasonal = FALSE)
res_yes <- run_one_seasonality(seasonal = TRUE)

# ---------------------------
# Consolidar comparaciones
# ---------------------------
irf_all <- dplyr::bind_rows(
  res_no$irf_v_ga,
  res_yes$irf_v_ga,
  res_no$irf_v_gw,
  res_yes$irf_v_gw,
  res_no$irf_ga_gw,
  res_yes$irf_ga_gw
) %>%
  dplyr::mutate(exclude_zero = exclude_zero(lower, upper))

mult_all <- dplyr::bind_rows(res_no$mult_ga, res_yes$mult_ga) %>%
  dplyr::mutate(exclude_zero = exclude_zero(lower, upper))

irf_summary <- summarise_irf(irf_all)

signif_compare <- irf_all %>%
  dplyr::select(model, impulse, response, h, irf, lower, upper, exclude_zero)

mult_compare <- mult_all %>%
  dplyr::select(model, impulse, response, H, multiplier, lower, upper, exclude_zero)

# Guardar tablas comparativas
readr::write_csv(irf_all,        file.path(dir_tbl, paste0("irf_all_models_", BASELINE_TAG, "_p", P_FIXED, ".csv")))
readr::write_csv(irf_summary,    file.path(dir_tbl, paste0("irf_summary_compare_", BASELINE_TAG, "_p", P_FIXED, ".csv")))
readr::write_csv(signif_compare, file.path(dir_tbl, paste0("irf_significance_compare_", BASELINE_TAG, "_p", P_FIXED, ".csv")))
readr::write_csv(mult_compare,   file.path(dir_tbl, paste0("cum_multiplier_compare_", BASELINE_TAG, "_p", P_FIXED, ".csv")))

# ---------------------------
# Figuras comparativas
# ---------------------------
p_v_ga <- plot_irf_compare(
  dplyr::filter(irf_all, impulse == "v", response == "ga"),
  "IRF comparada: shock empleo (v) -> productividad (ga)",
  "ga"
)
p_v_gw <- plot_irf_compare(
  dplyr::filter(irf_all, impulse == "v", response == "gw"),
  "IRF comparada: shock empleo (v) -> salarios (gw)",
  "gw"
)
p_ga_gw <- plot_irf_compare(
  dplyr::filter(irf_all, impulse == "ga", response == "gw"),
  "IRF comparada: shock productividad (ga) -> salarios (gw)",
  "gw"
)

ggplot2::ggsave(file.path(dir_fig, paste0("compare_irf_v_to_ga_", BASELINE_TAG, "_p", P_FIXED, ".pdf")), p_v_ga, width = 7, height = 4.5)
ggplot2::ggsave(file.path(dir_fig, paste0("compare_irf_v_to_gw_", BASELINE_TAG, "_p", P_FIXED, ".pdf")), p_v_gw, width = 7, height = 4.5)
ggplot2::ggsave(file.path(dir_fig, paste0("compare_irf_ga_to_gw_", BASELINE_TAG, "_p", P_FIXED, ".pdf")), p_ga_gw, width = 7, height = 4.5)

message("\n✔ 24c listo: revisa tables/svar_core/p4_fixed/seasonality_compare y figures/svar_core/p4_fixed/seasonality_compare\n")
