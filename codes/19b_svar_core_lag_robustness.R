# codes/19b_svar_core_lag_robustness.R
# ==========================================================
# 19b) Robustez por rezagos (baseline): p=1 vs p=6
# - Mantiene A0 teórico (definido en 18)
# - Guarda:
#   tables/svar_core/lag_robustness/*.csv
#   figures/svar_core/lag_robustness/*.pdf
# ==========================================================

source(here::here("codes","00_setup.R"))
message("\n== 19b) Robustez SVAR por rezagos (p=1 vs p=6) ==\n")

pkgs <- c("zoo", "tseries", "vars")
to_install <- pkgs[!vapply(pkgs, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)]
if (length(to_install) > 0) install.packages(to_install)

library(zoo)
library(tseries)
library(vars)

source(here::here("codes","18_svar_core_functions.R"))

# ---------------------------
# Output dirs (según tu setup)
# ---------------------------
dir_tbl <- as.character(fs::path(CFG$dirs$out_tables, "svar_core", "lag_robustness"))
dir_fig <- as.character(fs::path(CFG$dirs$out_figures, "svar_core", "lag_robustness"))
fs::dir_create(dir_tbl)
fs::dir_create(dir_fig)

# ---------------------------
# Inputs (del 17)
# ---------------------------
in_path <- fs::path(CFG$dirs$processed, "svar_core_inputs_2008Q1_2022Q4.rds")
stopifnot(file.exists(in_path))
df <- load_rds(in_path) %>%
  dplyr::mutate(q = zoo::as.yearqtr(paste0(year, " Q", trimestre), format = "%Y Q%q")) %>%
  dplyr::arrange(q)

# ---------------------------
# Helper: fit + IRFs para un p fijo
# ---------------------------
fit_one_p <- function(p_lag,
                      wage_col = "w_real_mean_winsor",
                      n_ahead = 12,
                      runs = 2000,
                      ci = 0.95,
                      seed = 123) {
  
  X <- build_core(df, wage_col = wage_col, a_col = "a_prod")
  Xmat <- X %>% dplyr::select(ga, gw, v)
  
  # VAR con p literal (blindaje bootstrap)
  var_fit <- eval(bquote(vars::VAR(Xmat, p = .(as.integer(p_lag)), type = "const")))
  
  # diagnósticos
  diag_tbl <- diagnostics_var(var_fit) %>%
    dplyr::mutate(p = as.integer(p_lag))
  
  # SVAR (A0 teórico; función ya blindada en 18)
  svar <- fit_svar_A0(var_fit)
  spec <- attr(svar, "svar_spec")
  spec_B <- if (!is.null(spec)) spec$B else NA_character_
  spec_method <- if (!is.null(spec)) spec$method else NA_character_
  
  # IRFs (bootstrap reproducible)
  set.seed(seed + as.integer(p_lag))
  irf_v <- vars::irf(
    svar,
    impulse = "v",
    response = c("ga", "gw"),
    n.ahead = n_ahead,
    boot = TRUE,
    ci = ci,
    runs = runs
  )
  
  irf_v_ga <- tidy_irf(irf_v, "v", "ga") %>% dplyr::mutate(p = as.integer(p_lag))
  irf_v_gw <- tidy_irf(irf_v, "v", "gw") %>% dplyr::mutate(p = as.integer(p_lag))
  
  set.seed(seed + 1000 + as.integer(p_lag))
  irf_ga <- vars::irf(
    svar,
    impulse = "ga",
    response = c("gw"),
    n.ahead = n_ahead,
    boot = TRUE,
    ci = ci,
    runs = runs
  )
  irf_ga_gw <- tidy_irf(irf_ga, "ga", "gw") %>% dplyr::mutate(p = as.integer(p_lag))
  
  # Multiplicador acumulado (ga ante shock v)
  mult <- cum_multiplier(irf_v_ga, H_vec = c(4, 8, 12)) %>%
    dplyr::mutate(p = as.integer(p_lag))
  
  list(
    diag = diag_tbl %>% dplyr::mutate(svar_B = spec_B, svar_method = spec_method),
    irf_v_ga = irf_v_ga,
    irf_v_gw = irf_v_gw,
    irf_ga_gw = irf_ga_gw,
    mult = mult
  )
}

# ---------------------------
# Correr p=1 y p=6 (baseline)
# ---------------------------
res_p1 <- fit_one_p(p_lag = 1)
res_p6 <- fit_one_p(p_lag = 6)

# ---------------------------
# Consolidar
# ---------------------------
diag_all <- dplyr::bind_rows(res_p1$diag, res_p6$diag) %>%
  dplyr::arrange(p)

irf_v_ga_all <- dplyr::bind_rows(res_p1$irf_v_ga, res_p6$irf_v_ga)
irf_v_gw_all <- dplyr::bind_rows(res_p1$irf_v_gw, res_p6$irf_v_gw)
irf_ga_gw_all <- dplyr::bind_rows(res_p1$irf_ga_gw, res_p6$irf_ga_gw)

mult_all <- dplyr::bind_rows(res_p1$mult, res_p6$mult) %>%
  dplyr::arrange(p, H)

# ---------------------------
# Guardar CSVs
# ---------------------------
readr::write_csv(diag_all, fs::path(dir_tbl, "diagnostics_p1_vs_p6.csv"))
readr::write_csv(mult_all, fs::path(dir_tbl, "cum_multiplier_p1_vs_p6.csv"))

readr::write_csv(irf_v_ga_all, fs::path(dir_tbl, "irf_v_to_ga_p1_vs_p6.csv"))
readr::write_csv(irf_v_gw_all, fs::path(dir_tbl, "irf_v_to_gw_p1_vs_p6.csv"))
readr::write_csv(irf_ga_gw_all, fs::path(dir_tbl, "irf_ga_to_gw_p1_vs_p6.csv"))

# ---------------------------
# Plots comparables (facet por p)
# ---------------------------
plot_irf_facet <- function(df_irf, title, ylab) {
  df_irf %>%
    dplyr::mutate(p = factor(p, levels = c(1,6), labels = c("p=1 (BIC/SC)", "p=6 (AIC)"))) %>%
    ggplot2::ggplot(ggplot2::aes(x = h, y = irf)) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.4) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), alpha = 0.20) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::facet_wrap(~p, ncol = 2) +
    ggplot2::labs(title = title, x = "Horizonte (trimestres)", y = ylab) +
    ggplot2::theme_bw(base_size = 12)
}

p1 <- plot_irf_facet(irf_v_ga_all, "IRF (baseline): shock empleo (v) -> productividad (ga)", "ga")
p2 <- plot_irf_facet(irf_v_gw_all, "IRF (baseline): shock empleo (v) -> salarios (gw)", "gw")
p3 <- plot_irf_facet(irf_ga_gw_all, "IRF (baseline): shock productividad (ga) -> salarios (gw)", "gw")

ggplot2::ggsave(filename = file.path(dir_fig, "irf_v_to_ga_p1_vs_p6.pdf"), plot = p1, width = 10, height = 4.5)
ggplot2::ggsave(filename = file.path(dir_fig, "irf_v_to_gw_p1_vs_p6.pdf"), plot = p2, width = 10, height = 4.5)
ggplot2::ggsave(filename = file.path(dir_fig, "irf_ga_to_gw_p1_vs_p6.pdf"), plot = p3, width = 10, height = 4.5)

message("✔ 19b listo: robustez por rezagos guardada en tables/svar_core/lag_robustness y figures/svar_core/lag_robustness")