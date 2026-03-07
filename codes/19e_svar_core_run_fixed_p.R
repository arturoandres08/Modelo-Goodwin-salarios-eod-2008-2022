# codes/19e_svar_core_run_fixed_p.R
# ==========================================================
# 19e) Resultados SVAR núcleo (ga, gw, v) — p fijo (p=4)
# - Baseline final (diagnóstico: mejora serial sin romper estabilidad)
# - Robustez salarial: mean winsor / mean sin winsor / mediana
# Outputs:
#   tables/svar_core/p4_fixed/*
#   figures/svar_core/p4_fixed/*
# ==========================================================

source(here::here("codes","00_setup.R"))
message("\n== 19e) SVAR núcleo: p fijo (p=4) + robustez salarial ==\n")

pkgs <- c("zoo", "tseries", "vars")
to_install <- pkgs[!vapply(pkgs, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)]
if (length(to_install) > 0) install.packages(to_install)

library(zoo)
library(tseries)
library(vars)

source(here::here("codes","18_svar_core_functions.R"))

# ---------------------------
# Parámetros globales
# ---------------------------
P_FIXED <- 4L
N_AHEAD <- 12
RUNS_BOOT <- 2000
CI_LEVEL <- 0.95
SEED0 <- 123

# ---------------------------
# Output dirs (según tu setup)
# ---------------------------
dir_tbl <- as.character(fs::path(CFG$dirs$out_tables, "svar_core", "p4_fixed"))
dir_fig <- as.character(fs::path(CFG$dirs$out_figures, "svar_core", "p4_fixed"))
fs::dir_create(dir_tbl)
fs::dir_create(dir_fig)

# ---------------------------
# Inputs del 17
# ---------------------------
in_path <- fs::path(CFG$dirs$processed, "svar_core_inputs_2008Q1_2022Q4.rds")
stopifnot(file.exists(in_path))
df <- load_rds(in_path) %>%
  dplyr::mutate(q = zoo::as.yearqtr(paste0(year, " Q", trimestre), format = "%Y Q%q")) %>%
  dplyr::arrange(q)

# ---------------------------
# Fit con p fijo
# ---------------------------
fit_fixed_p <- function(Xmat, p_fixed) {
  eval(bquote(vars::VAR(Xmat, p = .(as.integer(p_fixed)), type = "const")))
}

run_one_fixed <- function(wage_col, tag,
                          p_fixed = P_FIXED,
                          n_ahead = N_AHEAD,
                          runs = RUNS_BOOT,
                          ci = CI_LEVEL,
                          seed = SEED0) {
  
  message("\n--- RUN TAG: ", tag, " | wage_col: ", wage_col, " | p_fixed=", p_fixed, " ---")
  
  X <- build_core(df, wage_col = wage_col, a_col = "a_prod")
  Xmat <- X %>% dplyr::select(ga, gw, v)
  
  # Unit roots
  ur <- dplyr::bind_rows(
    unit_root_table(X$ga, paste0("ga (", tag, ", p=", p_fixed, ")")),
    unit_root_table(X$gw, paste0("gw (", tag, ", p=", p_fixed, ")")),
    unit_root_table(X$v,  paste0("v  (", tag, ", p=", p_fixed, ")"))
  )
  
  # VAR fijo + diagnósticos
  var_fit <- fit_fixed_p(Xmat, p_fixed = p_fixed)
  diag <- diagnostics_var(var_fit) %>%
    dplyr::mutate(tag = tag, p_used = as.integer(p_fixed))
  
  # SVAR (A0 teórico; fallback numérico B=I -> B diag libre)
  svar <- fit_svar_A0(var_fit)
  spec <- attr(svar, "svar_spec")
  if (!is.null(spec)) message("SVAR spec usado: B=", spec$B, " | method=", spec$method)
  
  # IRFs
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
  
  # Multiplicador acumulado (ga ante shock v)
  mult <- cum_multiplier(irf_v_ga, H_vec = c(4,8,12)) %>%
    dplyr::mutate(tag = tag, p_used = as.integer(p_fixed))
  
  # ---- Guardar tablas ----
  readr::write_csv(ur %>% dplyr::mutate(tag = tag, p_used = as.integer(p_fixed)),
                   file.path(dir_tbl, paste0("unit_root_", tag, "_p", p_fixed, ".csv")))
  
  readr::write_csv(diag,
                   file.path(dir_tbl, paste0("var_diagnostics_", tag, "_p", p_fixed, ".csv")))
  
  readr::write_csv(mult,
                   file.path(dir_tbl, paste0("cum_multiplier_", tag, "_p", p_fixed, ".csv")))
  
  readr::write_csv(irf_v_ga, file.path(dir_tbl, paste0("irf_v_to_ga_", tag, "_p", p_fixed, ".csv")))
  readr::write_csv(irf_v_gw, file.path(dir_tbl, paste0("irf_v_to_gw_", tag, "_p", p_fixed, ".csv")))
  readr::write_csv(irf_ga_gw, file.path(dir_tbl, paste0("irf_ga_to_gw_", tag, "_p", p_fixed, ".csv")))
  # ---- Figuras ----
  p1 <- plot_irf(irf_v_ga, paste0("IRF (p=", p_fixed, "): shock empleo (v) -> productividad (ga) [", tag, "]"), "ga")
  p2 <- plot_irf(irf_v_gw, paste0("IRF (p=", p_fixed, "): shock empleo (v) -> salarios (gw) [", tag, "]"), "gw")
  p3 <- plot_irf(irf_ga_gw, paste0("IRF (p=", p_fixed, "): shock productividad (ga) -> salarios (gw) [", tag, "]"), "gw")
  
  ggplot2::ggsave(filename = file.path(dir_fig, paste0("irf_v_to_ga_", tag, "_p", p_fixed, ".pdf")),
                  plot = p1, width = 7, height = 4.5)
  ggplot2::ggsave(filename = file.path(dir_fig, paste0("irf_v_to_gw_", tag, "_p", p_fixed, ".pdf")),
                  plot = p2, width = 7, height = 4.5)
  ggplot2::ggsave(filename = file.path(dir_fig, paste0("irf_ga_to_gw_", tag, "_p", p_fixed, ".pdf")),
                  plot = p3, width = 7, height = 4.5)
  
  invisible(list(tag=tag, p_used=p_fixed, diag=diag, mult=mult, svar=svar))
}

# ---------------------------
# Correr baseline + robustez salarial (p=4)
# ---------------------------
res_baseline <- run_one_fixed("w_real_mean_winsor",   "baseline_mean_winsor", p_fixed = P_FIXED)
res_nowinsor <- run_one_fixed("w_real_mean_nowinsor", "robust_mean_nowinsor", p_fixed = P_FIXED)
res_median   <- run_one_fixed("w_real_median",        "robust_median",        p_fixed = P_FIXED)

message("\n✔ 19e listo: revisa tables/svar_core/p4_fixed y figures/svar_core/p4_fixed\n")
