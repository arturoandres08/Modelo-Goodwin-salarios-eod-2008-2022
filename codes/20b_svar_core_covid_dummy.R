# codes/20b_svar_core_covid_dummy.R
# ==========================================================
# 20b) Robustez COVID — Dummy COVID en el VAR (determinística exógena)
# - VAR con exogen: dummy=1 en 2020Q2–2021Q1 (editable)
# - Compara IRFs vs baseline sin dummy
# Outputs:
#   tables/svar_core/covid_dummy/*
#   figures/svar_core/covid_dummy/*
# ==========================================================

source(here::here("codes","00_setup.R"))
message("\n== 20b) Robustez COVID: dummy como exógena en VAR ==\n")

pkgs <- c("zoo", "vars", "tseries")
to_install <- pkgs[!vapply(pkgs, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)]
if (length(to_install) > 0) install.packages(to_install)

library(zoo)
library(vars)
library(tseries)

source(here::here("codes","18_svar_core_functions.R"))

# ---- parámetros ----
P_FIXED   <- 4L
N_AHEAD   <- 12
RUNS_BOOT <- 2000
CI_LEVEL  <- 0.95
SEED0     <- 123

# dummy window (puedes cambiarlo)
Q0 <- zoo::as.yearqtr("2020 Q2", format = "%Y Q%q")
Q1 <- zoo::as.yearqtr("2021 Q1", format = "%Y Q%q")

# ---- outputs ----
dir_tbl <- as.character(fs::path(CFG$dirs$out_tables, "svar_core", "covid_dummy"))
dir_fig <- as.character(fs::path(CFG$dirs$out_figures, "svar_core", "covid_dummy"))
fs::dir_create(dir_tbl)
fs::dir_create(dir_fig)

# ---- inputs ----
in_path <- fs::path(CFG$dirs$processed, "svar_core_inputs_2008Q1_2022Q4.rds")
stopifnot(file.exists(in_path))

df0 <- load_rds(in_path) %>%
  dplyr::mutate(q = zoo::as.yearqtr(paste0(year, " Q", trimestre), format = "%Y Q%q")) %>%
  dplyr::arrange(q)

# ---- helper: VAR p fijo con exogen literal (para bootstrap) ----
fit_var_fixed_p_exogen <- function(Xmat, p_fixed, exogen_mat = NULL) {
  if (is.null(exogen_mat)) {
    return(eval(bquote(vars::VAR(Xmat, p = .(as.integer(p_fixed)), type = "const"))))
  }
  eval(bquote(vars::VAR(Xmat, p = .(as.integer(p_fixed)), type = "const", exogen = .(exogen_mat))))
}

run_svar_dummy <- function(df_in,
                           sample_tag,
                           wage_col = "w_real_mean_winsor",
                           include_dummy = FALSE,
                           p_fixed = P_FIXED,
                           n_ahead = N_AHEAD,
                           runs = RUNS_BOOT,
                           ci = CI_LEVEL,
                           seed = SEED0) {
  
  X <- build_core(df_in, wage_col = wage_col, a_col = "a_prod")
  Xmat <- X %>% dplyr::select(ga, gw, v)
  
  # Exógena alineada a X (no a df original), para que calce con diff/log y drop_na
  exo <- NULL
  if (include_dummy) {
    covid_dummy <- as.integer(X$q >= Q0 & X$q <= Q1)
    exo <- matrix(covid_dummy, ncol = 1)
    colnames(exo) <- "D_COVID"
  }
  
  var_fit <- fit_var_fixed_p_exogen(Xmat, p_fixed = p_fixed, exogen_mat = exo)
  
  diag <- diagnostics_var(var_fit) %>%
    dplyr::mutate(sample = sample_tag, p_used = as.integer(p_fixed), dummy = include_dummy)
  
  svar <- fit_svar_A0(var_fit)
  spec <- attr(svar, "svar_spec")
  specB <- if (!is.null(spec)) spec$B else NA_character_
  specM <- if (!is.null(spec)) spec$method else NA_character_
  
  set.seed(seed)
  irf_v <- vars::irf(svar, impulse = "v", response = c("ga","gw"),
                     n.ahead = n_ahead, boot = TRUE, ci = ci, runs = runs)
  irf_v_ga <- tidy_irf(irf_v, "v", "ga") %>%
    dplyr::mutate(sample = sample_tag, dummy = include_dummy, p_used = as.integer(p_fixed))
  irf_v_gw <- tidy_irf(irf_v, "v", "gw") %>%
    dplyr::mutate(sample = sample_tag, dummy = include_dummy, p_used = as.integer(p_fixed))
  
  set.seed(seed + 1)
  irf_ga <- vars::irf(svar, impulse = "ga", response = c("gw"),
                      n.ahead = n_ahead, boot = TRUE, ci = ci, runs = runs)
  irf_ga_gw <- tidy_irf(irf_ga, "ga", "gw") %>%
    dplyr::mutate(sample = sample_tag, dummy = include_dummy, p_used = as.integer(p_fixed))
  
  mult <- cum_multiplier(irf_v_ga, H_vec = c(4,8,12)) %>%
    dplyr::mutate(sample = sample_tag, dummy = include_dummy, p_used = as.integer(p_fixed))
  
  list(
    diag = diag %>% dplyr::mutate(svar_B = specB, svar_method = specM),
    irf_v_ga = irf_v_ga,
    irf_v_gw = irf_v_gw,
    irf_ga_gw = irf_ga_gw,
    mult = mult
  )
}

# ---- correr: baseline sin dummy vs con dummy ----
res_base  <- run_svar_dummy(df0, sample_tag = "Sin dummy (baseline)", include_dummy = FALSE)
res_dummy <- run_svar_dummy(df0, sample_tag = "Con dummy COVID",       include_dummy = TRUE)

diag_all <- dplyr::bind_rows(res_base$diag, res_dummy$diag)
mult_all <- dplyr::bind_rows(res_base$mult, res_dummy$mult)

irf_v_ga_all  <- dplyr::bind_rows(res_base$irf_v_ga,  res_dummy$irf_v_ga)
irf_v_gw_all  <- dplyr::bind_rows(res_base$irf_v_gw,  res_dummy$irf_v_gw)
irf_ga_gw_all <- dplyr::bind_rows(res_base$irf_ga_gw, res_dummy$irf_ga_gw)

# ---- guardar ----
readr::write_csv(diag_all, fs::path(dir_tbl, "diagnostics_baseline_vs_dummy.csv"))
readr::write_csv(mult_all, fs::path(dir_tbl, "cum_multiplier_baseline_vs_dummy.csv"))

readr::write_csv(irf_v_ga_all,  fs::path(dir_tbl, "irf_v_to_ga_baseline_vs_dummy.csv"))
readr::write_csv(irf_v_gw_all,  fs::path(dir_tbl, "irf_v_to_gw_baseline_vs_dummy.csv"))
readr::write_csv(irf_ga_gw_all, fs::path(dir_tbl, "irf_ga_to_gw_baseline_vs_dummy.csv"))

# ---- plots ----
plot_irf_facet <- function(df_irf, title, ylab) {
  ggplot2::ggplot(df_irf, ggplot2::aes(x = h, y = irf)) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.4) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), alpha = 0.20) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::facet_wrap(~sample, ncol = 2) +
    ggplot2::labs(title = title, x = "Horizonte (trimestres)", y = ylab) +
    ggplot2::theme_bw(base_size = 12)
}

p1 <- plot_irf_facet(irf_v_ga_all,  "IRF baseline (p=4): shock empleo (v) -> productividad (ga) | dummy COVID", "ga")
p2 <- plot_irf_facet(irf_v_gw_all,  "IRF baseline (p=4): shock empleo (v) -> salarios (gw) | dummy COVID", "gw")
p3 <- plot_irf_facet(irf_ga_gw_all, "IRF baseline (p=4): shock productividad (ga) -> salarios (gw) | dummy COVID", "gw")

ggplot2::ggsave(filename = file.path(dir_fig, "irf_v_to_ga_baseline_vs_dummy.pdf"), plot = p1, width = 10, height = 4.5)
ggplot2::ggsave(filename = file.path(dir_fig, "irf_v_to_gw_baseline_vs_dummy.pdf"), plot = p2, width = 10, height = 4.5)
ggplot2::ggsave(filename = file.path(dir_fig, "irf_ga_to_gw_baseline_vs_dummy.pdf"), plot = p3, width = 10, height = 4.5)

message("✔ 20b listo: ver tables/svar_core/covid_dummy y figures/svar_core/covid_dummy")