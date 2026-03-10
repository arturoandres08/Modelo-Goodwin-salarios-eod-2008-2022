# codes/24b_irf_confidence_sensitivity_p4_fixed.R
# ==========================================================
# 24b) Sensibilidad de IRFs a distintos niveles de confianza
#      para el baseline final con p fijo = 4
#
# Objetivo:
# - Mantener intacto el baseline oficial de 19e
# - Reestimar SOLO el baseline_mean_winsor con la misma
#   identificación SVAR núcleo (ga, gw, v)
# - Reportar IRFs para varios niveles de confianza:
#   68%, 90%, 95%, 99% y 99.9%
# - Guardar tablas, figuras y una tabla-resumen de robustez
#
# Outputs:
# - tables/svar_core/p4_fixed/ci_sensitivity/*
# - figures/svar_core/p4_fixed/ci_sensitivity/*
# ==========================================================

source(here::here("codes", "00_setup.R"))

message("\n== 24b) Sensibilidad de IRFs a niveles de confianza (p fijo = 4) ==\n")

pkgs <- c("zoo", "tseries", "vars", "purrr", "stringr")
to_install <- pkgs[!vapply(pkgs, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)]
if (length(to_install) > 0) install.packages(to_install)

library(zoo)
library(tseries)
library(vars)
library(purrr)
library(stringr)

source(here::here("codes", "18_svar_core_functions.R"))

# ---------------------------
# Parámetros globales
# ---------------------------
P_FIXED   <- 4L
N_AHEAD   <- 12L
RUNS_BOOT <- 2000L
SEED0     <- 123L
CI_LEVELS <- c(0.68, 0.90, 0.95, 0.99, 0.999)
BASELINE_WAGE_COL <- "w_real_mean_winsor"
BASELINE_TAG      <- "baseline_mean_winsor"

# ---------------------------
# Directorios de salida
# ---------------------------
out_tbl <- fs::path(CFG$dirs$out_tables, "svar_core", "p4_fixed", "ci_sensitivity")
out_fig <- fs::path(CFG$dirs$out_figures, "svar_core", "p4_fixed", "ci_sensitivity")
fs::dir_create(out_tbl)
fs::dir_create(out_fig)

# ---------------------------
# Helpers
# ---------------------------
fmt_ci <- function(ci) {
  if (isTRUE(all.equal(ci, 0.999))) return("999")
  sprintf("%02d", round(ci * 100))
}

fmt_ci_label <- function(ci) {
  pct <- if (isTRUE(all.equal(ci, 0.999))) "99.9" else format(round(ci * 100, 1), trim = TRUE)
  paste0(pct, "%")
}

fit_fixed_p <- function(Xmat, p_fixed) {
  eval(bquote(vars::VAR(Xmat, p = .(as.integer(p_fixed)), type = "const")))
}

cum_irf_path <- function(df_irf) {
  df_irf %>%
    dplyr::arrange(h) %>%
    dplyr::mutate(
      cum_irf   = cumsum(irf),
      cum_lower = cumsum(lower),
      cum_upper = cumsum(upper)
    )
}

exclude_zero <- function(lower, upper) {
  is.finite(lower) & is.finite(upper) & ((lower > 0 & upper > 0) | (lower < 0 & upper < 0))
}

sign_label <- function(x) {
  dplyr::case_when(
    !is.finite(x) ~ NA_character_,
    x > 0 ~ "positivo",
    x < 0 ~ "negativo",
    TRUE  ~ "cero"
  )
}

add_sig_columns <- function(df_irf) {
  df_irf %>%
    dplyr::mutate(
      ci_excludes_zero = exclude_zero(lower, upper),
      effect_sign      = sign_label(irf),
      band_sign        = dplyr::case_when(
        lower > 0 & upper > 0 ~ "positivo",
        lower < 0 & upper < 0 ~ "negativo",
        TRUE ~ "incluye_cero"
      )
    )
}

plot_irf_ci <- function(df_irf, title_txt, ylab_txt) {
  ggplot2::ggplot(df_irf, ggplot2::aes(x = h, y = irf)) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.3) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), alpha = 0.20) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::labs(
      title = title_txt,
      x = "Horizonte",
      y = ylab_txt
    ) +
    ggplot2::theme_minimal(base_size = 11)
}

# ---------------------------
# Inputs del 17
# ---------------------------
in_path <- fs::path(CFG$dirs$processed, "svar_core_inputs_2008Q1_2022Q4.rds")
stopifnot(file.exists(in_path))

df <- load_rds(in_path) %>%
  dplyr::mutate(q = zoo::as.yearqtr(paste0(year, " Q", trimestre), format = "%Y Q%q")) %>%
  dplyr::arrange(q)

# ---------------------------
# Construcción baseline y diagnósticos únicos
# ---------------------------
X <- build_core(df, wage_col = BASELINE_WAGE_COL, a_col = "a_prod")
Xmat <- X %>% dplyr::select(ga, gw, v)

ur <- dplyr::bind_rows(
  unit_root_table(X$ga, paste0("ga (", BASELINE_TAG, ", p=", P_FIXED, ")")),
  unit_root_table(X$gw, paste0("gw (", BASELINE_TAG, ", p=", P_FIXED, ")")),
  unit_root_table(X$v,  paste0("v (",  BASELINE_TAG, ", p=", P_FIXED, ")"))
) %>%
  dplyr::mutate(tag = BASELINE_TAG, p_used = as.integer(P_FIXED))

var_fit <- fit_fixed_p(Xmat, p_fixed = P_FIXED)

diag_tbl <- diagnostics_var(var_fit) %>%
  dplyr::mutate(tag = BASELINE_TAG, p_used = as.integer(P_FIXED))

svar_fit <- fit_svar_A0(var_fit)
spec <- attr(svar_fit, "svar_spec")
if (!is.null(spec)) {
  message("SVAR spec usado: B=", spec$B, " | method=", spec$method)
}

readr::write_csv(ur, fs::path(out_tbl, "unit_root_baseline_mean_winsor_p4.csv"))
readr::write_csv(diag_tbl, fs::path(out_tbl, "var_diagnostics_baseline_mean_winsor_p4.csv"))

# ---------------------------
# Runner por nivel de confianza
# ---------------------------
run_one_ci <- function(ci, seed_base = SEED0) {
  ci_tag   <- fmt_ci(ci)
  ci_label <- fmt_ci_label(ci)

  message("\n--- CI = ", ci_label, " (tag: ci", ci_tag, ") ---")

  set.seed(seed_base)
  irf_v <- vars::irf(
    svar_fit,
    impulse = "v",
    response = c("ga", "gw"),
    n.ahead = N_AHEAD,
    boot = TRUE,
    ci = ci,
    runs = RUNS_BOOT
  )

  irf_v_ga <- tidy_irf(irf_v, "v", "ga") %>%
    add_sig_columns() %>%
    dplyr::mutate(
      impulse = "v",
      response = "ga",
      ci = ci,
      ci_label = ci_label,
      ci_tag = ci_tag,
      tag = BASELINE_TAG,
      p_used = as.integer(P_FIXED)
    )

  irf_v_gw <- tidy_irf(irf_v, "v", "gw") %>%
    add_sig_columns() %>%
    dplyr::mutate(
      impulse = "v",
      response = "gw",
      ci = ci,
      ci_label = ci_label,
      ci_tag = ci_tag,
      tag = BASELINE_TAG,
      p_used = as.integer(P_FIXED)
    )

  set.seed(seed_base + 1L)
  irf_ga <- vars::irf(
    svar_fit,
    impulse = "ga",
    response = c("gw"),
    n.ahead = N_AHEAD,
    boot = TRUE,
    ci = ci,
    runs = RUNS_BOOT
  )

  irf_ga_gw <- tidy_irf(irf_ga, "ga", "gw") %>%
    add_sig_columns() %>%
    dplyr::mutate(
      impulse = "ga",
      response = "gw",
      ci = ci,
      ci_label = ci_label,
      ci_tag = ci_tag,
      tag = BASELINE_TAG,
      p_used = as.integer(P_FIXED)
    )

  mult_ga <- cum_multiplier(irf_v_ga, H_vec = c(4, 8, 12)) %>%
    dplyr::transmute(
      H = H,
      multiplier = M,
      lower = M_lower,
      upper = M_upper,
      impulse = "v",
      response = "ga",
      ci = ci,
      ci_label = ci_label,
      ci_tag = ci_tag,
      tag = BASELINE_TAG,
      p_used = as.integer(P_FIXED)
    )

  cum_gw <- cum_irf_path(irf_v_gw) %>%
    dplyr::mutate(
      impulse = "v",
      response = "gw",
      ci = ci,
      ci_label = ci_label,
      ci_tag = ci_tag,
      tag = BASELINE_TAG,
      p_used = as.integer(P_FIXED)
    )

  mult_gw <- cum_gw %>%
    dplyr::filter(h %in% c(4, 8, 12)) %>%
    dplyr::transmute(
      H = h,
      multiplier = cum_irf,
      lower = cum_lower,
      upper = cum_upper,
      ci = ci,
      ci_label = ci_label,
      ci_tag = ci_tag,
      impulse = "v",
      response = "gw",
      tag = BASELINE_TAG,
      p_used = as.integer(P_FIXED)
    )

  # Guardar CSV por CI
  readr::write_csv(irf_v_ga, fs::path(out_tbl, paste0("irf_v_to_ga_", BASELINE_TAG, "_p4_ci", ci_tag, ".csv")))
  readr::write_csv(irf_v_gw, fs::path(out_tbl, paste0("irf_v_to_gw_", BASELINE_TAG, "_p4_ci", ci_tag, ".csv")))
  readr::write_csv(irf_ga_gw, fs::path(out_tbl, paste0("irf_ga_to_gw_", BASELINE_TAG, "_p4_ci", ci_tag, ".csv")))
  readr::write_csv(mult_ga, fs::path(out_tbl, paste0("cum_multiplier_v_to_ga_", BASELINE_TAG, "_p4_ci", ci_tag, ".csv")))
  readr::write_csv(cum_gw, fs::path(out_tbl, paste0("cum_irf_v_to_gw_", BASELINE_TAG, "_p4_ci", ci_tag, ".csv")))
  readr::write_csv(mult_gw, fs::path(out_tbl, paste0("cum_multiplier_v_to_gw_", BASELINE_TAG, "_p4_ci", ci_tag, ".csv")))

  # Figuras por CI
  p1 <- plot_irf_ci(
    irf_v_ga,
    paste0("IRF (p=4, CI ", ci_label, "): shock en v -> ga [", BASELINE_TAG, "]"),
    "ga"
  )
  p2 <- plot_irf_ci(
    irf_v_gw,
    paste0("IRF (p=4, CI ", ci_label, "): shock en v -> gw [", BASELINE_TAG, "]"),
    "gw"
  )
  p3 <- plot_irf_ci(
    irf_ga_gw,
    paste0("IRF (p=4, CI ", ci_label, "): shock en ga -> gw [", BASELINE_TAG, "]"),
    "gw"
  )

  ggplot2::ggsave(
    filename = fs::path(out_fig, paste0("irf_v_to_ga_", BASELINE_TAG, "_p4_ci", ci_tag, ".pdf")),
    plot = p1, width = 7, height = 4.5
  )
  ggplot2::ggsave(
    filename = fs::path(out_fig, paste0("irf_v_to_gw_", BASELINE_TAG, "_p4_ci", ci_tag, ".pdf")),
    plot = p2, width = 7, height = 4.5
  )
  ggplot2::ggsave(
    filename = fs::path(out_fig, paste0("irf_ga_to_gw_", BASELINE_TAG, "_p4_ci", ci_tag, ".pdf")),
    plot = p3, width = 7, height = 4.5
  )

  list(
    irf_v_ga = irf_v_ga,
    irf_v_gw = irf_v_gw,
    irf_ga_gw = irf_ga_gw,
    mult_ga = mult_ga,
    cum_gw = cum_gw,
    mult_gw = mult_gw
  )
}

# ---------------------------
# Correr todos los niveles de confianza
# ---------------------------
res_ci <- purrr::map(CI_LEVELS, run_one_ci)
names(res_ci) <- paste0("ci", vapply(CI_LEVELS, fmt_ci, character(1)))

# ---------------------------
# Consolidado largo
# ---------------------------
all_irf_v_ga <- purrr::map_dfr(res_ci, "irf_v_ga")
all_irf_v_gw <- purrr::map_dfr(res_ci, "irf_v_gw")
all_irf_ga_gw <- purrr::map_dfr(res_ci, "irf_ga_gw")
all_mult_ga <- purrr::map_dfr(res_ci, "mult_ga")
all_cum_gw <- purrr::map_dfr(res_ci, "cum_gw")
all_mult_gw <- purrr::map_dfr(res_ci, "mult_gw")

readr::write_csv(all_irf_v_ga, fs::path(out_tbl, "irf_v_to_ga_all_ci_baseline_mean_winsor_p4.csv"))
readr::write_csv(all_irf_v_gw, fs::path(out_tbl, "irf_v_to_gw_all_ci_baseline_mean_winsor_p4.csv"))
readr::write_csv(all_irf_ga_gw, fs::path(out_tbl, "irf_ga_to_gw_all_ci_baseline_mean_winsor_p4.csv"))
readr::write_csv(all_mult_ga, fs::path(out_tbl, "cum_multiplier_v_to_ga_all_ci_baseline_mean_winsor_p4.csv"))
readr::write_csv(all_cum_gw, fs::path(out_tbl, "cum_irf_v_to_gw_all_ci_baseline_mean_winsor_p4.csv"))
readr::write_csv(all_mult_gw, fs::path(out_tbl, "cum_multiplier_v_to_gw_all_ci_baseline_mean_winsor_p4.csv"))

# ---------------------------
# Tabla-resumen: hasta qué CI excluye cero cada horizonte
# ---------------------------
make_robustness_grid <- function(df_irf, impulse_name, response_name) {
  wide_sig <- df_irf %>%
    dplyr::select(h, ci_label, ci_excludes_zero) %>%
    dplyr::mutate(ci_col = paste0("robust_", stringr::str_replace_all(ci_label, c("%" = "", "\\." = "_")))) %>%
    dplyr::select(-ci_label) %>%
    tidyr::pivot_wider(names_from = ci_col, values_from = ci_excludes_zero)

  df_irf %>%
    dplyr::group_by(h) %>%
    dplyr::summarise(
      impulse = impulse_name,
      response = response_name,
      irf_sign = dplyr::first(sign_label(irf)),
      max_ci_robust = {
        hit <- ci[ci_excludes_zero]
        if (length(hit) == 0) NA_real_ else max(hit)
      },
      max_ci_robust_label = {
        hit <- ci_label[ci_excludes_zero]
        if (length(hit) == 0) "ninguno" else ci_label[which.max(ci)]
      },
      .groups = "drop"
    ) %>%
    dplyr::left_join(wide_sig, by = "h") %>%
    dplyr::arrange(h)
}

sum_v_ga <- make_robustness_grid(all_irf_v_ga, "v", "ga")
sum_v_gw <- make_robustness_grid(all_irf_v_gw, "v", "gw")
sum_ga_gw <- make_robustness_grid(all_irf_ga_gw, "ga", "gw")

summary_irf_robustness <- dplyr::bind_rows(sum_v_ga, sum_v_gw, sum_ga_gw) %>%
  dplyr::mutate(
    tag = BASELINE_TAG,
    p_used = as.integer(P_FIXED)
  )

readr::write_csv(summary_irf_robustness, fs::path(out_tbl, "summary_irf_robustness_by_horizon_baseline_mean_winsor_p4.csv"))

# ---------------------------
# Tabla-resumen: multiplicadores acumulados H = 4, 8, 12
# ---------------------------
make_multiplier_grid <- function(df_mult, impulse_name, response_name) {
  wide_sig <- df_mult %>%
    dplyr::mutate(ci_excludes_zero = exclude_zero(lower, upper)) %>%
    dplyr::select(H, ci_label, ci_excludes_zero) %>%
    dplyr::mutate(ci_col = paste0("robust_", stringr::str_replace_all(ci_label, c("%" = "", "\\." = "_")))) %>%
    dplyr::select(-ci_label) %>%
    tidyr::pivot_wider(names_from = ci_col, values_from = ci_excludes_zero)

  df_mult %>%
    dplyr::mutate(ci_excludes_zero = exclude_zero(lower, upper)) %>%
    dplyr::group_by(H) %>%
    dplyr::summarise(
      impulse = impulse_name,
      response = response_name,
      multiplier_sign = dplyr::first(sign_label(multiplier)),
      max_ci_robust = {
        hit <- ci[ci_excludes_zero]
        if (length(hit) == 0) NA_real_ else max(hit)
      },
      max_ci_robust_label = {
        hit <- ci_label[ci_excludes_zero]
        if (length(hit) == 0) "ninguno" else ci_label[which.max(ci)]
      },
      .groups = "drop"
    ) %>%
    dplyr::left_join(wide_sig, by = "H") %>%
    dplyr::arrange(H)
}

summary_multiplier_ga <- make_multiplier_grid(all_mult_ga, "v", "ga")
summary_multiplier_gw <- make_multiplier_grid(all_mult_gw, "v", "gw")
summary_multiplier <- dplyr::bind_rows(summary_multiplier_ga, summary_multiplier_gw) %>%
  dplyr::mutate(
    tag = BASELINE_TAG,
    p_used = as.integer(P_FIXED)
  )

readr::write_csv(summary_multiplier, fs::path(out_tbl, "summary_cum_multipliers_robustness_baseline_mean_winsor_p4.csv"))

message("\n✔ 24b listo. Revisa:\n",
        " - tables/svar_core/p4_fixed/ci_sensitivity\n",
        " - figures/svar_core/p4_fixed/ci_sensitivity\n")
