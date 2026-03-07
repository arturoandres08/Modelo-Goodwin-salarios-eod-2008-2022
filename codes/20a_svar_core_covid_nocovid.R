# codes/20a_svar_core_covid_nocovid.R
# ==========================================================
# 20a) Robustez COVID — Submuestra noCOVID
# - Estima SVAR baseline (p fijo) en:
#   (i) muestra completa 2008Q1–2022Q4
#   (ii) submuestra noCOVID 2008Q1–2019Q4
# - Compara IRFs (v->ga, v->gw, ga->gw)
# Outputs:
#   tables/svar_core/covid_nocovid/*
#   figures/svar_core/covid_nocovid/*
# ==========================================================

source(here::here("codes","00_setup.R"))
message("\n== 20a) Robustez COVID: submuestra noCOVID (2008Q1–2019Q4) ==\n")

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

# ---- outputs ----
dir_tbl <- as.character(fs::path(CFG$dirs$out_tables, "svar_core", "covid_nocovid"))
dir_fig <- as.character(fs::path(CFG$dirs$out_figures, "svar_core", "covid_nocovid"))
fs::dir_create(dir_tbl)
fs::dir_create(dir_fig)

# ---- inputs ----
in_path <- fs::path(CFG$dirs$processed, "svar_core_inputs_2008Q1_2022Q4.rds")
stopifnot(file.exists(in_path))

df0 <- load_rds(in_path) %>%
  dplyr::mutate(q = zoo::as.yearqtr(paste0(year, " Q", trimestre), format = "%Y Q%q")) %>%
  dplyr::arrange(q)

# submuestra noCOVID
q_end_nocovid <- zoo::as.yearqtr("2019 Q4", format = "%Y Q%q")
df_nocovid <- df0 %>% dplyr::filter(q <= q_end_nocovid)

# ---- helper: VAR p fijo (p literal para bootstrap) ----
fit_var_fixed_p <- function(Xmat, p_fixed) {
  eval(bquote(vars::VAR(Xmat, p = .(as.integer(p_fixed)), type = "const")))
}

# ---- helper: correr un SVAR baseline y devolver IRFs tidy ----
run_svar_irfs <- function(df_in, sample_tag,
                          wage_col = "w_real_mean_winsor",
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
  
  svar <- fit_svar_A0(var_fit)
  spec <- attr(svar, "svar_spec")
  specB <- if (!is.null(spec)) spec$B else NA_character_
  specM <- if (!is.null(spec)) spec$method else NA_character_
  
  # IRFs
  set.seed(seed)
  irf_v <- vars::irf(svar, impulse = "v", response = c("ga","gw"),
                     n.ahead = n_ahead, boot = TRUE, ci = ci, runs = runs)
  irf_v_ga <- tidy_irf(irf_v, "v", "ga") %>% dplyr::mutate(sample = sample_tag, p_used = as.integer(p_fixed))
  irf_v_gw <- tidy_irf(irf_v, "v", "gw") %>% dplyr::mutate(sample = sample_tag, p_used = as.integer(p_fixed))
  
  set.seed(seed + 1)
  irf_ga <- vars::irf(svar, impulse = "ga", response = c("gw"),
                      n.ahead = n_ahead, boot = TRUE, ci = ci, runs = runs)
  irf_ga_gw <- tidy_irf(irf_ga, "ga", "gw") %>% dplyr::mutate(sample = sample_tag, p_used = as.integer(p_fixed))
  
  mult <- cum_multiplier(irf_v_ga, H_vec = c(4,8,12)) %>%
    dplyr::mutate(sample = sample_tag, p_used = as.integer(p_fixed))
  
  list(
    diag = diag %>% dplyr::mutate(svar_B = specB, svar_method = specM),
    irf_v_ga = irf_v_ga,
    irf_v_gw = irf_v_gw,
    irf_ga_gw = irf_ga_gw,
    mult = mult
  )
}

# ---- correr: full vs noCOVID ----
res_full    <- run_svar_irfs(df0,       sample_tag = "Full (2008Q1–2022Q4)")
res_nocovid <- run_svar_irfs(df_nocovid, sample_tag = "noCOVID (2008Q1–2019Q4)")

diag_all <- dplyr::bind_rows(res_full$diag, res_nocovid$diag)
mult_all <- dplyr::bind_rows(res_full$mult, res_nocovid$mult)

irf_v_ga_all  <- dplyr::bind_rows(res_full$irf_v_ga,  res_nocovid$irf_v_ga)
irf_v_gw_all  <- dplyr::bind_rows(res_full$irf_v_gw,  res_nocovid$irf_v_gw)
irf_ga_gw_all <- dplyr::bind_rows(res_full$irf_ga_gw, res_nocovid$irf_ga_gw)

# ---- guardar CSV ----
readr::write_csv(diag_all, fs::path(dir_tbl, "diagnostics_full_vs_nocovid.csv"))
readr::write_csv(mult_all, fs::path(dir_tbl, "cum_multiplier_full_vs_nocovid.csv"))

readr::write_csv(irf_v_ga_all,  fs::path(dir_tbl, "irf_v_to_ga_full_vs_nocovid.csv"))
readr::write_csv(irf_v_gw_all,  fs::path(dir_tbl, "irf_v_to_gw_full_vs_nocovid.csv"))
readr::write_csv(irf_ga_gw_all, fs::path(dir_tbl, "irf_ga_to_gw_full_vs_nocovid.csv"))

# ---- plots (facet por muestra) ----
plot_irf_facet <- function(df_irf, title, ylab) {
  ggplot2::ggplot(df_irf, ggplot2::aes(x = h, y = irf)) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.4) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), alpha = 0.20) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::facet_wrap(~sample, ncol = 2) +
    ggplot2::labs(title = title, x = "Horizonte (trimestres)", y = ylab) +
    ggplot2::theme_bw(base_size = 12)
}

p1 <- plot_irf_facet(irf_v_ga_all,  "IRF baseline (p=4): shock empleo (v) -> productividad (ga)", "ga")
p2 <- plot_irf_facet(irf_v_gw_all,  "IRF baseline (p=4): shock empleo (v) -> salarios (gw)", "gw")
p3 <- plot_irf_facet(irf_ga_gw_all, "IRF baseline (p=4): shock productividad (ga) -> salarios (gw)", "gw")

ggplot2::ggsave(filename = file.path(dir_fig, "irf_v_to_ga_full_vs_nocovid.pdf"), plot = p1, width = 10, height = 4.5)
ggplot2::ggsave(filename = file.path(dir_fig, "irf_v_to_gw_full_vs_nocovid.pdf"), plot = p2, width = 10, height = 4.5)
ggplot2::ggsave(filename = file.path(dir_fig, "irf_ga_to_gw_full_vs_nocovid.pdf"), plot = p3, width = 10, height = 4.5)

message("✔ 20a listo: ver tables/svar_core/covid_nocovid y figures/svar_core/covid_nocovid")