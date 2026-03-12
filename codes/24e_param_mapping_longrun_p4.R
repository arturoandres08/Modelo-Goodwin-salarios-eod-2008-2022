# codes/24e_param_mapping_longrun_p4.R
# ==========================================================
# 24e) Param mapping exploratorio de largo plazo (baseline p=4)
#
# Objetivo:
#   Construir alpha(H), rho(H) y rho_minus_alpha(H) usando
#   draws bootstrap comparables de la misma corrida baseline:
#     alpha(H)           := M_{v -> ga}(H)
#     rho(H)             := M_{v -> gw}(H)
#     rho_minus_alpha(H) := rho(H) - alpha(H)
#
# Decisiones congeladas:
#   - baseline_mean_winsor
#   - wage_col = w_real_mean_winsor
#   - p = 4
#   - H principal = 12
#   - sensibilidad = 8
#   - n.ahead = 12
#   - bootstrap = 2000
#   - seed = 123
#
# Outputs:
#   tables/svar_core/p4_fixed/param_mapping/
#     - param_mapping_draws_baseline_mean_winsor_p4.rds
#     - param_mapping_summary_H8_H12_baseline_mean_winsor_p4.csv
#     - param_mapping_table_baseline_mean_winsor_p4.tex
#     - proxy_gw_to_v_longrun_baseline_mean_winsor_p4.csv
#     - bootstrap_failures_baseline_mean_winsor_p4.csv
# ==========================================================

source(here::here("codes", "00_setup.R"))

message("\n== 24e) Param mapping long run baseline p=4 ==\n")

pkgs <- c("zoo", "vars", "tseries", "dplyr", "readr", "purrr", "tidyr", "tibble", "knitr")
to_install <- pkgs[!vapply(pkgs, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)]
if (length(to_install) > 0) install.packages(to_install)

library(zoo)
library(vars)
library(tseries)
library(dplyr)
library(readr)
library(purrr)
library(tidyr)
library(tibble)
library(knitr)

source(here::here("codes", "18_svar_core_functions.R"))

# ----------------------------------------------------------
# Parámetros congelados
# ----------------------------------------------------------
TAG        <- "baseline_mean_winsor"
WAGE_COL   <- "w_real_mean_winsor"
P_FIXED    <- 4L
N_AHEAD    <- 12L
RUNS_BOOT  <- 2000L
SEED0      <- 123L
H_TARGET   <- c(8L, 12L)

REUSE_IF_EXISTS <- TRUE

# ----------------------------------------------------------
# Directorios
# ----------------------------------------------------------
dir_tbl <- as.character(fs::path(CFG$dirs$out_tables, "svar_core", "p4_fixed", "param_mapping"))
fs::dir_create(dir_tbl)

draws_rds_path <- file.path(dir_tbl, "param_mapping_draws_baseline_mean_winsor_p4.rds")
summary_csv_path <- file.path(dir_tbl, "param_mapping_summary_H8_H12_baseline_mean_winsor_p4.csv")
table_tex_path <- file.path(dir_tbl, "param_mapping_table_baseline_mean_winsor_p4.tex")
proxy_csv_path <- file.path(dir_tbl, "proxy_gw_to_v_longrun_baseline_mean_winsor_p4.csv")
fail_csv_path  <- file.path(dir_tbl, "bootstrap_failures_baseline_mean_winsor_p4.csv")

# ----------------------------------------------------------
# Cargar input baseline igual que 19e
# ----------------------------------------------------------
in_path <- fs::path(CFG$dirs$processed, "svar_core_inputs_2008Q1_2022Q4.rds")
stopifnot(file.exists(in_path))

df <- load_rds(in_path) %>%
  dplyr::mutate(
    q = zoo::as.yearqtr(paste0(year, " Q", trimestre), format = "%Y Q%q")
  ) %>%
  dplyr::arrange(q)

X <- build_core(df, wage_col = WAGE_COL, a_col = "a_prod")
Xmat <- X %>% dplyr::select(ga, gw, v)

fit_fixed_p <- function(Xmat, p_fixed) {
  eval(bquote(vars::VAR(Xmat, p = .(as.integer(p_fixed)), type = "const")))
}

var_fit <- fit_fixed_p(Xmat, p_fixed = P_FIXED)
svar_fit <- fit_svar_A0(var_fit)

spec <- attr(svar_fit, "svar_spec")
if (!is.null(spec)) {
  message("SVAR spec usado: B=", spec$B, " | method=", spec$method)
}

# ----------------------------------------------------------
# Helpers
# ----------------------------------------------------------
get_irf_path_no_boot <- function(svar_obj, impulse, responses, n_ahead = N_AHEAD) {
  irf_obj <- vars::irf(
    svar_obj,
    impulse = impulse,
    response = responses,
    n.ahead = n_ahead,
    boot = FALSE
  )

  purrr::map_dfr(responses, function(resp) {
    irf_mat <- irf_obj$irf[[impulse]]
    if (is.null(dim(irf_mat))) {
      vals <- as.numeric(irf_mat)
    } else {
      vals <- as.numeric(irf_mat[, resp])
    }

    tibble::tibble(
      impulse = impulse,
      response = resp,
      h = 0:(length(vals) - 1L),
      irf = vals
    ) %>%
      dplyr::mutate(
        relation = paste0(impulse, "_to_", response),
        cum_irf = cumsum(irf)
      )
  })
}

extract_const_vec <- function(var_model) {
  var_names <- colnames(var_model$y)
  out <- sapply(var_names, function(vn) {
    cf <- stats::coef(var_model$varresult[[vn]])
    nm <- names(cf)

    idx_nonlag <- !grepl("\\.l[0-9]+$", nm)
    if (!any(idx_nonlag)) return(0)

    unname(cf[which(idx_nonlag)[1]])
  })
  as.numeric(out)
}

get_resid_matrix <- function(var_model) {
  rr <- tryCatch(stats::residuals(var_model), error = function(e) NULL)

  if (is.matrix(rr)) {
    rr <- as.matrix(rr)
    colnames(rr) <- colnames(var_model$y)
    return(rr)
  }

  var_names <- colnames(var_model$y)
  out <- sapply(var_names, function(vn) stats::residuals(var_model$varresult[[vn]]))
  out <- as.matrix(out)
  colnames(out) <- var_names
  out
}

simulate_var_bootstrap_sample <- function(var_model) {
  Y <- as.matrix(var_model$y)
  colnames(Y) <- colnames(var_model$y)

  p <- as.integer(var_model$p)
  K <- ncol(Y)
  Tn <- nrow(Y)

  A_list <- vars::Acoef(var_model)
  const_vec <- extract_const_vec(var_model)
  resid_mat <- get_resid_matrix(var_model)

  stopifnot(nrow(resid_mat) == (Tn - p))

  draw_idx <- sample.int(nrow(resid_mat), size = nrow(resid_mat), replace = TRUE)
  U_star <- resid_mat[draw_idx, , drop = FALSE]

  Y_star <- matrix(NA_real_, nrow = Tn, ncol = K)
  colnames(Y_star) <- colnames(Y)

  # condiciones iniciales: primeras p observaciones reales
  Y_star[1:p, ] <- Y[1:p, , drop = FALSE]

  for (tt in (p + 1):Tn) {
    pred <- const_vec

    for (L in seq_len(p)) {
      pred <- pred + as.numeric(A_list[[L]] %*% Y_star[tt - L, ])
    }

    Y_star[tt, ] <- pred + U_star[tt - p, ]
  }

  as.data.frame(Y_star)
}

make_draw_row <- function(b, irf_df) {
  irf_df %>%
    dplyr::mutate(
      b = as.integer(b),
      tag = TAG,
      wage_col = WAGE_COL,
      p_used = as.integer(P_FIXED)
    ) %>%
    dplyr::select(b, tag, wage_col, p_used, impulse, response, relation, h, irf, cum_irf)
}

summarise_boot <- function(x) {
  qs <- stats::quantile(
    x,
    probs = c(0.16, 0.84, 0.05, 0.95, 0.025, 0.975),
    na.rm = TRUE,
    names = FALSE
  )

  tibble::tibble(
    boot_median = stats::median(x, na.rm = TRUE),
    ci68_lo = qs[1],
    ci68_hi = qs[2],
    ci90_lo = qs[3],
    ci90_hi = qs[4],
    ci95_lo = qs[5],
    ci95_hi = qs[6],
    n_success = sum(is.finite(x))
  )
}

format_ci <- function(lo, hi, digits = 4) {
  paste0("[", formatC(lo, digits = digits, format = "f"),
         ", ", formatC(hi, digits = digits, format = "f"), "]")
}

# ----------------------------------------------------------
# Punto base (sin bootstrap)
# ----------------------------------------------------------
point_v <- get_irf_path_no_boot(svar_fit, impulse = "v", responses = c("ga", "gw"), n_ahead = N_AHEAD)
point_proxy <- tryCatch(
  get_irf_path_no_boot(svar_fit, impulse = "gw", responses = c("v"), n_ahead = N_AHEAD),
  error = function(e) NULL
)

point_H <- point_v %>%
  dplyr::filter(h %in% H_TARGET) %>%
  dplyr::select(relation, h, point_estimate = cum_irf)

point_alpha <- point_H %>%
  dplyr::filter(relation == "v_to_ga") %>%
  dplyr::transmute(H = h, parameter = "alpha", point_estimate)

point_rho <- point_H %>%
  dplyr::filter(relation == "v_to_gw") %>%
  dplyr::transmute(H = h, parameter = "rho", point_estimate)

point_rho_minus_alpha <- point_alpha %>%
  dplyr::inner_join(point_rho, by = "H", suffix = c("_alpha", "_rho")) %>%
  dplyr::transmute(
    H = H,
    parameter = "rho_minus_alpha",
    point_estimate = point_estimate_rho - point_estimate_alpha
  )

point_params <- dplyr::bind_rows(point_alpha, point_rho, point_rho_minus_alpha)

# ----------------------------------------------------------
# Bootstrap draw-by-draw (reutiliza si ya existe)
# ----------------------------------------------------------
if (REUSE_IF_EXISTS && file.exists(draws_rds_path)) {
  message("Reutilizando draws existentes en: ", draws_rds_path)
  draws_obj <- readRDS(draws_rds_path)
  draws_long <- draws_obj$draws_long
  param_draws <- draws_obj$param_draws
  proxy_draws <- draws_obj$proxy_draws
  fail_tbl <- draws_obj$failures
} else {

  message("Iniciando bootstrap manual baseline. Esto puede tardar varios minutos...")

  draws_list <- vector("list", RUNS_BOOT)
  proxy_list <- vector("list", RUNS_BOOT)
  fail_log <- vector("list", RUNS_BOOT)

  set.seed(SEED0)

  for (b in seq_len(RUNS_BOOT)) {
    if (b %% 50L == 0L) {
      message("Bootstrap draw ", b, " / ", RUNS_BOOT)
    }

    ans <- tryCatch({

      Y_star <- simulate_var_bootstrap_sample(var_fit)
      var_b <- fit_fixed_p(as.matrix(Y_star), p_fixed = P_FIXED)
      svar_b <- fit_svar_A0(var_b)

      irf_v_b <- get_irf_path_no_boot(
        svar_b,
        impulse = "v",
        responses = c("ga", "gw"),
        n_ahead = N_AHEAD
      )

      out_draw <- make_draw_row(b = b, irf_df = irf_v_b)

      proxy_b <- tryCatch(
        get_irf_path_no_boot(
          svar_b,
          impulse = "gw",
          responses = c("v"),
          n_ahead = N_AHEAD
        ) %>%
          make_draw_row(b = b, irf_df = .),
        error = function(e) NULL
      )

      list(draw = out_draw, proxy = proxy_b)

    }, error = function(e) {
      fail_log[[b]] <<- tibble::tibble(
        b = as.integer(b),
        error = conditionMessage(e)
      )
      NULL
    })

    if (!is.null(ans)) {
      draws_list[[b]] <- ans$draw
      proxy_list[[b]] <- ans$proxy
    }
  }

  draws_long <- dplyr::bind_rows(draws_list)
  proxy_draws <- dplyr::bind_rows(proxy_list)
  fail_tbl <- dplyr::bind_rows(fail_log)

  # construir alpha, rho y rho-alpha draw por draw en H={8,12}
  alpha_draws <- draws_long %>%
    dplyr::filter(relation == "v_to_ga", h %in% H_TARGET) %>%
    dplyr::transmute(b, H = h, alpha = cum_irf)

  rho_draws <- draws_long %>%
    dplyr::filter(relation == "v_to_gw", h %in% H_TARGET) %>%
    dplyr::transmute(b, H = h, rho = cum_irf)

  param_draws <- alpha_draws %>%
    dplyr::inner_join(rho_draws, by = c("b", "H")) %>%
    dplyr::mutate(rho_minus_alpha = rho - alpha) %>%
    tidyr::pivot_longer(
      cols = c(alpha, rho, rho_minus_alpha),
      names_to = "parameter",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      tag = TAG,
      wage_col = WAGE_COL,
      p_used = as.integer(P_FIXED)
    ) %>%
    dplyr::select(tag, wage_col, p_used, b, H, parameter, value)

  draws_obj <- list(
    meta = list(
      tag = TAG,
      wage_col = WAGE_COL,
      p_used = as.integer(P_FIXED),
      n_ahead = as.integer(N_AHEAD),
      runs_boot = as.integer(RUNS_BOOT),
      seed = as.integer(SEED0),
      H_target = H_TARGET,
      svar_spec = spec
    ),
    point_irf_v = point_v,
    point_proxy = point_proxy,
    draws_long = draws_long,
    param_draws = param_draws,
    proxy_draws = proxy_draws,
    failures = fail_tbl
  )

  saveRDS(draws_obj, draws_rds_path)
}

# ----------------------------------------------------------
# Resumen principal H = 8, 12
# ----------------------------------------------------------
summary_tbl <- param_draws %>%
  dplyr::group_by(parameter, H) %>%
  dplyr::summarise(summarise_boot(value), .groups = "drop") %>%
  dplyr::left_join(point_params, by = c("parameter", "H")) %>%
  dplyr::select(
    parameter, H, point_estimate, boot_median,
    ci68_lo, ci68_hi, ci90_lo, ci90_hi, ci95_lo, ci95_hi, n_success
  ) %>%
  dplyr::arrange(factor(parameter, levels = c("alpha", "rho_minus_alpha", "rho")), H)

readr::write_csv(summary_tbl, summary_csv_path)

# ----------------------------------------------------------
# Proxy complementaria gw -> v
# ----------------------------------------------------------
if (nrow(proxy_draws) > 0L && !is.null(point_proxy)) {

  point_proxy_H <- point_proxy %>%
    dplyr::filter(h %in% H_TARGET) %>%
    dplyr::transmute(H = h, point_estimate = cum_irf)

  proxy_summary <- proxy_draws %>%
    dplyr::filter(relation == "gw_to_v", h %in% H_TARGET) %>%
    dplyr::group_by(H = h) %>%
    dplyr::summarise(summarise_boot(cum_irf), .groups = "drop") %>%
    dplyr::left_join(point_proxy_H, by = "H") %>%
    dplyr::mutate(
      parameter = "proxy_gw_to_v",
      interpretation = "Proxy complementaria sugerida por el profesor; no usar como identificacion principal de rho_minus_alpha."
    ) %>%
    dplyr::select(parameter, H, point_estimate, boot_median,
                  ci68_lo, ci68_hi, ci90_lo, ci90_hi, ci95_lo, ci95_hi,
                  n_success, interpretation)

  readr::write_csv(proxy_summary, proxy_csv_path)

} else {
  proxy_summary <- tibble::tibble(
    parameter = character(),
    H = integer(),
    point_estimate = numeric(),
    boot_median = numeric(),
    ci68_lo = numeric(),
    ci68_hi = numeric(),
    ci90_lo = numeric(),
    ci90_hi = numeric(),
    ci95_lo = numeric(),
    ci95_hi = numeric(),
    n_success = integer(),
    interpretation = character()
  )
}

# ----------------------------------------------------------
# Tabla LaTeX corta para tesis/apéndice
# ----------------------------------------------------------
table_out <- summary_tbl %>%
  dplyr::mutate(
    parameter = dplyr::recode(
      parameter,
      alpha = "$\\alpha(H)$",
      rho = "$\\rho(H)$",
      rho_minus_alpha = "$\\rho(H)-\\alpha(H)$"
    ),
    `Estimador puntual` = formatC(point_estimate, digits = 4, format = "f"),
    `Mediana bootstrap` = formatC(boot_median, digits = 4, format = "f"),
    `IC 68\\%` = format_ci(ci68_lo, ci68_hi),
    `IC 90\\%` = format_ci(ci90_lo, ci90_hi),
    `IC 95\\%` = format_ci(ci95_lo, ci95_hi)
  ) %>%
  dplyr::select(
    Parametro = parameter,
    H,
    `Estimador puntual`,
    `Mediana bootstrap`,
    `IC 68\\%`,
    `IC 90\\%`,
    `IC 95\\%`
  )

tex_txt <- knitr::kable(
  table_out,
  format = "latex",
  booktabs = TRUE,
  escape = FALSE,
  align = c("l", "c", "r", "r", "r", "r", "r"),
  caption = "Mapeo paramétrico exploratorio de largo plazo: baseline mean winsor, p=4."
)

writeLines(tex_txt, con = table_tex_path)

# ----------------------------------------------------------
# Fallas bootstrap
# ----------------------------------------------------------
readr::write_csv(fail_tbl, fail_csv_path)

# ----------------------------------------------------------
# Mensajes finales
# ----------------------------------------------------------
message("\nArchivos guardados en: ", dir_tbl)
message(" - ", basename(draws_rds_path))
message(" - ", basename(summary_csv_path))
message(" - ", basename(table_tex_path))
if (nrow(proxy_summary) > 0L) message(" - ", basename(proxy_csv_path))
message(" - ", basename(fail_csv_path))

message("\nNota: este bloque debe leerse como mapeo paramétrico exploratorio.")
