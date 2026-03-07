# codes/22_svar_core_robust_delta_v.R
# ==========================================================
# 22) Robustez: reemplazar v_t por Δv_t (baseline SVAR p=4)
# - Baseline: v en nivel
# - Robustez: Δv = v - lag(v)
# - Mantiene A0 teórico (fit_svar_A0 desde 18)
# - Outputs:
#   tables/svar_core/delta_v/*
#   figures/svar_core/delta_v/*
# ==========================================================

source(here::here("codes","00_setup.R"))
message("\n== 22) Robustez SVAR: v_t vs Δv_t (p=4) ==\n")

pkgs <- c("zoo", "vars", "tseries")
to_install <- pkgs[!vapply(pkgs, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)]
if (length(to_install) > 0) install.packages(to_install)

library(zoo)
library(vars)
library(tseries)

# Usa build_core(), diagnostics_var(), fit_svar_A0(), tidy_irf(), plot_irf(), cum_multiplier(), unit_root_table()
source(here::here("codes","18_svar_core_functions.R"))

# ---------------------------
# Parámetros
# ---------------------------
P_FIXED   <- 4L
N_AHEAD   <- 12
RUNS_BOOT <- 2000
CI_LEVEL  <- 0.95
SEED0     <- 123

WAGE_COL  <- "w_real_mean_winsor"    # baseline salarial (puedes cambiar si quieres)
TAG_BASE  <- "Baseline (v level)"
TAG_DV    <- "Robustez (delta v)"

# ---------------------------
# Dirs output
# ---------------------------
dir_tbl <- as.character(fs::path(CFG$dirs$out_tables,  "svar_core", "delta_v"))
dir_fig <- as.character(fs::path(CFG$dirs$out_figures, "svar_core", "delta_v"))
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

stopifnot("v" %in% names(df0), WAGE_COL %in% names(df0), "a_prod" %in% names(df0))

# ---------------------------
# Helper: VAR p fijo (p literal para bootstrap)
# ---------------------------
fit_var_fixed_p <- function(Xmat, p_fixed) {
  eval(bquote(vars::VAR(Xmat, p = .(as.integer(p_fixed)), type = "const")))
}

# ---------------------------
# Run SVAR + IRFs (v->ga, v->gw, ga->gw) + mult
# ---------------------------
run_svar_irfs_fixedp <- function(df_in, sample_tag,
                                 wage_col = WAGE_COL,
                                 p_fixed = P_FIXED,
                                 n_ahead = N_AHEAD,
                                 runs = RUNS_BOOT,
                                 ci = CI_LEVEL,
                                 seed = SEED0) {
  
  X <- build_core(df_in, wage_col = wage_col, a_col = "a_prod")
  Xmat <- X %>% dplyr::select(ga, gw, v)
  
  var_fit <- fit_var_fixed_p(Xmat, p_fixed)
  
  diag <- diagnostics_var(var_fit) %>%
    dplyr::mutate(sample = sample_tag, p_used = as.integer(p_fixed))
  
  # SVAR A0 teórico
  svar <- fit_svar_A0(var_fit)
  spec <- attr(svar, "svar_spec")
  specB <- if (!is.null(spec)) spec$B else NA_character_
  specM <- if (!is.null(spec)) spec$method else NA_character_
  
  # IRFs
  set.seed(seed)
  irf_v <- vars::irf(
    svar, impulse = "v", response = c("ga","gw"),
    n.ahead = n_ahead, boot = TRUE, ci = ci, runs = runs
  )
  irf_v_ga <- tidy_irf(irf_v, "v", "ga") %>% dplyr::mutate(sample = sample_tag, p_used = as.integer(p_fixed))
  irf_v_gw <- tidy_irf(irf_v, "v", "gw") %>% dplyr::mutate(sample = sample_tag, p_used = as.integer(p_fixed))
  
  set.seed(seed + 1)
  irf_ga <- vars::irf(
    svar, impulse = "ga", response = c("gw"),
    n.ahead = n_ahead, boot = TRUE, ci = ci, runs = runs
  )
  irf_ga_gw <- tidy_irf(irf_ga, "ga", "gw") %>% dplyr::mutate(sample = sample_tag, p_used = as.integer(p_fixed))
  
  mult <- cum_multiplier(irf_v_ga, H_vec = c(4,8,12)) %>%
    dplyr::mutate(sample = sample_tag, p_used = as.integer(p_fixed))
  
  list(
    diag = diag %>% dplyr::mutate(svar_B = specB, svar_method = specM),
    irf_v_ga = irf_v_ga,
    irf_v_gw = irf_v_gw,
    irf_ga_gw = irf_ga_gw,
    mult = mult,
    X = X
  )
}

# ---------------------------
# Construir df con Δv (reemplaza columna v)
# Nota: Δv cambia interpretación: shock es "cambio del empleo"
# ---------------------------
df_dv <- df0 %>%
  dplyr::arrange(q) %>%
  dplyr::mutate(dv = v - dplyr::lag(v)) %>%
  dplyr::filter(is.finite(dv)) %>%
  dplyr::mutate(v = dv) %>%
  dplyr::select(-dv)

# ---------------------------
# Ejecutar baseline vs Δv
# ---------------------------
res_base <- run_svar_irfs_fixedp(df0,   sample_tag = TAG_BASE)
res_dv   <- run_svar_irfs_fixedp(df_dv, sample_tag = TAG_DV)

# ---------------------------
# Guardar diagnósticos y mult
# ---------------------------
readr::write_csv(dplyr::bind_rows(res_base$diag, res_dv$diag),
                 fs::path(dir_tbl, "diagnostics_baseline_vs_delta_v.csv"))

readr::write_csv(dplyr::bind_rows(res_base$mult, res_dv$mult),
                 fs::path(dir_tbl, "cum_multiplier_baseline_vs_delta_v.csv"))

# ---------------------------
# Guardar IRFs (CSV)
# ---------------------------
readr::write_csv(dplyr::bind_rows(res_base$irf_v_ga, res_dv$irf_v_ga),
                 fs::path(dir_tbl, "irf_v_to_ga_baseline_vs_delta_v.csv"))
readr::write_csv(dplyr::bind_rows(res_base$irf_v_gw, res_dv$irf_v_gw),
                 fs::path(dir_tbl, "irf_v_to_gw_baseline_vs_delta_v.csv"))
readr::write_csv(dplyr::bind_rows(res_base$irf_ga_gw, res_dv$irf_ga_gw),
                 fs::path(dir_tbl, "irf_ga_to_gw_baseline_vs_delta_v.csv"))

# ---------------------------
# (Opcional pero recomendado) Unit roots: v vs Δv
# ---------------------------
ur_v  <- unit_root_table(df0$v,  "v (nivel)",  adf_type = "drift", kpss_type = "mu")
ur_dv <- unit_root_table(df_dv$v, "Δv",        adf_type = "drift", kpss_type = "mu")

readr::write_csv(dplyr::bind_rows(ur_v, ur_dv),
                 fs::path(dir_tbl, "unit_root_v_vs_delta_v.csv"))

# ---------------------------
# Figuras comparativas (facet por muestra)
# ---------------------------
plot_irf_facet <- function(df_irf, title, ylab) {
  df_irf %>%
    dplyr::mutate(sample = factor(sample, levels = c(TAG_BASE, TAG_DV))) %>%
    ggplot2::ggplot(ggplot2::aes(x = h, y = irf)) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.35) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), alpha = 0.20, linewidth = 0) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::facet_wrap(~sample, ncol = 2) +
    ggplot2::scale_x_continuous(breaks = seq(0, 12, 2)) +
    ggplot2::labs(title = title, x = "Horizonte (trimestres)", y = ylab) +
    ggplot2::theme_bw(base_size = 12)
}

df_v_ga  <- dplyr::bind_rows(res_base$irf_v_ga,  res_dv$irf_v_ga)
df_v_gw  <- dplyr::bind_rows(res_base$irf_v_gw,  res_dv$irf_v_gw)
df_ga_gw <- dplyr::bind_rows(res_base$irf_ga_gw, res_dv$irf_ga_gw)

p1 <- plot_irf_facet(df_v_ga,  "IRF (p=4): shock v -> ga | Baseline vs delta v", "Respuesta (ga)")
p2 <- plot_irf_facet(df_v_gw,  "IRF (p=4): shock v -> gw | Baseline vs delta v", "Respuesta (gw)")
p3 <- plot_irf_facet(df_ga_gw, "IRF (p=4): shock ga -> gw | Baseline vs delta v", "Respuesta (gw)")

ggplot2::ggsave(filename = file.path(dir_fig, "irf_v_to_ga_baseline_vs_delta_v.pdf"), plot = p1, width = 10, height = 4.5)
ggplot2::ggsave(filename = file.path(dir_fig, "irf_v_to_gw_baseline_vs_delta_v.pdf"), plot = p2, width = 10, height = 4.5)
ggplot2::ggsave(filename = file.path(dir_fig, "irf_ga_to_gw_baseline_vs_delta_v.pdf"), plot = p3, width = 10, height = 4.5)

ggplot2::ggsave(filename = file.path(dir_fig, "irf_v_to_ga_baseline_vs_delta_v.png"), plot = p1, width = 10, height = 4.5, dpi = 300)
ggplot2::ggsave(filename = file.path(dir_fig, "irf_v_to_gw_baseline_vs_delta_v.png"), plot = p2, width = 10, height = 4.5, dpi = 300)
ggplot2::ggsave(filename = file.path(dir_fig, "irf_ga_to_gw_baseline_vs_delta_v.png"), plot = p3, width = 10, height = 4.5, dpi = 300)

message("✔ 22 listo: ver tables/svar_core/delta_v y figures/svar_core/delta_v")