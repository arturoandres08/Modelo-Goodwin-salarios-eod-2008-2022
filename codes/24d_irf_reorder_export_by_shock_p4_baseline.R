# codes/24d_irf_reorder_export_by_shock_p4_baseline.R
# ==========================================================
# 24d) Reorganizar y reexportar IRFs definitivas por tipo de shock
# Especificación base:
#   - baseline_mean_winsor
#   - p = 4
#   - muestra completa
#   - misma identificación SVAR del baseline
#
# Salidas:
#   - figures/svar_core/irf_reordered_p4_baseline/*
#   - tables/svar_core/irf_reordered_p4_baseline/*
#
# Exporta:
#   1) Figuras por bloque de shock:
#      - shock v:  v->v, v->ga, v->gw
#      - shock gw: gw->gw, gw->v, gw->ga
#      - shock ga: ga->ga, ga->gw, ga->v
#      cada una en CI 90% y 95%
#   2) CSV por bloque con estimación puntual y bandas 90/95
#   3) Tabla resumen de significancia al 90% y 95%
#   4) CSV largo interno con 68/90/95
# ==========================================================

source(here::here("codes", "00_setup.R"))

message("\n== 24d) Reorganización y reexportación de IRFs por shock ==\n")

pkgs <- c("zoo", "tseries", "vars", "dplyr", "readr", "ggplot2", "tidyr", "purrr", "stringr")
to_install <- pkgs[!vapply(pkgs, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)]
if (length(to_install) > 0) install.packages(to_install)

library(zoo)
library(tseries)
library(vars)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(purrr)
library(stringr)

source(here::here("codes", "18_svar_core_functions.R"))

# ----------------------------------------------------------
# Parámetros
# ----------------------------------------------------------
TAG       <- "baseline_mean_winsor"
WAGE_COL  <- "w_real_mean_winsor"
P_FIXED   <- 4L
N_AHEAD   <- 12
RUNS_BOOT <- 2000
SEED0     <- 123
CI_PLOT   <- c(0.90, 0.95)
CI_ALL    <- c(0.68, 0.90, 0.95)

# ----------------------------------------------------------
# Directorios de salida
# ----------------------------------------------------------
dir_tbl <- as.character(fs::path(CFG$dirs$out_tables, "svar_core", "irf_reordered_p4_baseline"))
dir_fig <- as.character(fs::path(CFG$dirs$out_figures, "svar_core", "irf_reordered_p4_baseline"))
fs::dir_create(dir_tbl)
fs::dir_create(dir_fig)

# ----------------------------------------------------------
# Input baseline
# ----------------------------------------------------------
in_path <- fs::path(CFG$dirs$processed, "svar_core_inputs_2008Q1_2022Q4.rds")
stopifnot(file.exists(in_path))

df <- load_rds(in_path) %>%
  dplyr::mutate(
    q = zoo::as.yearqtr(paste0(year, " Q", trimestre), format = "%Y Q%q")
  ) %>%
  dplyr::arrange(q)

# ----------------------------------------------------------
# Fit baseline idéntico a 19e
# ----------------------------------------------------------
fit_fixed_p <- function(Xmat, p_fixed) {
  eval(bquote(vars::VAR(Xmat, p = .(as.integer(p_fixed)), type = "const")))
}

X <- build_core(df, wage_col = WAGE_COL, a_col = "a_prod")
Xmat <- X %>% dplyr::select(ga, gw, v)

var_fit <- fit_fixed_p(Xmat, p_fixed = P_FIXED)
svar <- fit_svar_A0(var_fit)
spec <- attr(svar, "svar_spec")

if (!is.null(spec)) {
  message("SVAR spec usado: B=", spec$B, " | method=", spec$method)
}

# ----------------------------------------------------------
# Bloques y etiquetas
# ----------------------------------------------------------
shock_blocks <- list(
  v  = c("v",  "ga", "gw"),
  gw = c("gw", "v",  "ga"),
  ga = c("ga", "gw", "v")
)

shock_titles <- c(
  v  = "Shock laboral",
  gw = "Shock salarial",
  ga = "Shock de productividad"
)

response_labels <- c(
  v  = "Empleo (v)",
  gw = "Salarios reales (gw)",
  ga = "Productividad (ga)"
)

pair_labels <- function(impulse, response) {
  paste0(impulse, " -> ", response)
}

ci_tag <- function(ci) {
  paste0("ci", gsub("\\.", "", sprintf("%.2f", ci)))
}

# ----------------------------------------------------------
# Extraer IRFs por shock / CI
# ----------------------------------------------------------
extract_one_irf <- function(svar_obj, impulse, responses, ci, seed) {
  set.seed(seed)
  irf_obj <- vars::irf(
    svar_obj,
    impulse = impulse,
    response = responses,
    n.ahead = N_AHEAD,
    boot = TRUE,
    ci = ci,
    runs = RUNS_BOOT
  )

  purrr::map_dfr(responses, function(resp) {
    tidy_irf(irf_obj, impulse, resp) %>%
      dplyr::mutate(
        impulse = impulse,
        response = resp,
        relation = pair_labels(impulse, resp),
        ci = ci,
        ci_label = paste0(round(ci * 100), "%"),
        ci_tag = ci_tag(ci),
        tag = TAG,
        wage_col = WAGE_COL,
        p_used = as.integer(P_FIXED)
      )
  })
}

all_irf <- purrr::imap_dfr(shock_blocks, function(responses, impulse_name) {
  purrr::map2_dfr(
    CI_ALL,
    seq_along(CI_ALL),
    function(ci, j) {
      extract_one_irf(
        svar_obj = svar,
        impulse = impulse_name,
        responses = responses,
        ci = ci,
        seed = SEED0 + 100 * j + match(impulse_name, names(shock_blocks))
      ) %>%
        dplyr::mutate(
          block = paste0("shock_", impulse_name),
          block_title = shock_titles[[impulse_name]]
        )
    }
  )
}) %>%
  dplyr::arrange(impulse, response, ci, h)

# ----------------------------------------------------------
# CSV por bloque con estimate + bandas 90/95 (+68 opcional)
# ----------------------------------------------------------
make_block_csv <- function(df_block) {
  est_tbl <- df_block %>%
    dplyr::group_by(h, response) %>%
    dplyr::summarise(
      estimate = dplyr::first(irf),
      .groups = "drop"
    )

  band_tbl <- df_block %>%
    dplyr::mutate(ci_num = paste0(round(ci * 100))) %>%
    dplyr::select(h, response, ci_num, lower, upper) %>%
    tidyr::pivot_wider(
      names_from = ci_num,
      values_from = c(lower, upper),
      names_sep = "_"
    )

  est_tbl %>%
    dplyr::left_join(band_tbl, by = c("h", "response")) %>%
    dplyr::rename(
      horizonte = h,
      respuesta = response,
      estimacion_puntual = estimate,
      limite_inferior_68 = lower_68,
      limite_superior_68 = upper_68,
      limite_inferior_90 = lower_90,
      limite_superior_90 = upper_90,
      limite_inferior_95 = lower_95,
      limite_superior_95 = upper_95
    ) %>%
    dplyr::arrange(base::match(respuesta, unique(df_block$response)), horizonte)
}

for (blk in names(shock_blocks)) {
  block_name <- paste0("shock_", blk)
  out_csv <- all_irf %>%
    dplyr::filter(block == block_name)

  block_csv <- make_block_csv(out_csv)

  readr::write_csv(
    block_csv,
    file.path(
      dir_tbl,
      paste0("irf_block_", block_name, "_", TAG, "_p", P_FIXED, ".csv")
    )
  )
}

# ----------------------------------------------------------
# Tabla resumen de significancia 90/95
# ----------------------------------------------------------
sig_summary <- all_irf %>%
  dplyr::filter(ci %in% c(0.90, 0.95)) %>%
  dplyr::mutate(
    excludes_zero = !(lower <= 0 & upper >= 0),
    ci_level = paste0(round(ci * 100))
  ) %>%
  dplyr::group_by(block, impulse, response, relation, ci_level) %>%
  dplyr::summarise(
    first_h_sig = {
      idx <- which(excludes_zero)
      if (length(idx) == 0) NA_integer_ else as.integer(h[min(idx)])
    },
    horizons_sig = {
      hh <- h[excludes_zero]
      if (length(hh) == 0) "" else paste(hh, collapse = ", ")
    },
    any_sig = any(excludes_zero),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from = ci_level,
    values_from = c(first_h_sig, horizons_sig, any_sig),
    names_glue = "{.value}_ci{ci_level}"
  ) %>%
  dplyr::arrange(base::match(block, c("shock_v", "shock_gw", "shock_ga")),
                 base::match(response, c("v", "ga", "gw", "gw", "v", "ga")))

readr::write_csv(
  sig_summary,
  file.path(dir_tbl, paste0("irf_significance_summary_ci90_ci95_", TAG, "_p", P_FIXED, ".csv"))
)

# Tabla larga útil para revisar todas las bandas
readr::write_csv(
  all_irf,
  file.path(dir_tbl, paste0("irf_all_long_ci68_ci90_ci95_", TAG, "_p", P_FIXED, ".csv"))
)

# ----------------------------------------------------------
# Gráficos por bloque y CI
# ----------------------------------------------------------
plot_block <- function(df_plot, impulse_name, ci_value) {

  resp_order <- shock_blocks[[impulse_name]]

  df_plot <- df_plot %>%
    dplyr::mutate(
      response = factor(response, levels = resp_order),
      response_title = factor(
        response_labels[as.character(response)],
        levels = response_labels[resp_order]
      )
    )

  lim_val <- max(abs(c(df_plot$lower, df_plot$upper, df_plot$irf)), na.rm = TRUE)
  if (!is.finite(lim_val) || lim_val <= 0) lim_val <- 0.1
  lim_val <- 1.05 * lim_val

  title_txt <- paste0(
    shock_titles[[impulse_name]],
    " — IRFs baseline mean winsor, p=4, CI ", round(ci_value * 100), "%"
  )

  ggplot(df_plot, aes(x = h, y = irf)) +
    geom_hline(yintercept = 0, linewidth = 0.35) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.20) +
    geom_line(linewidth = 0.8) +
    facet_wrap(~response_title, ncol = 3, scales = "fixed") +
    coord_cartesian(ylim = c(-lim_val, lim_val)) +
    labs(
      title = title_txt,
      subtitle = paste0("Shock estructural en ", impulse_name),
      x = "Horizonte (trimestres)",
      y = "Respuesta"
    ) +
    theme_bw(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      strip.text = element_text(face = "bold")
    )
}

for (impulse_name in names(shock_blocks)) {
  for (ci_value in CI_PLOT) {
    df_plot <- all_irf %>%
      dplyr::filter(impulse == impulse_name, ci == ci_value)

    p <- plot_block(df_plot, impulse_name = impulse_name, ci_value = ci_value)

    out_name <- paste0(
      "irf_panel_shock_", impulse_name, "_",
      ci_tag(ci_value), "_", TAG, "_p", P_FIXED, ".pdf"
    )

    ggplot2::ggsave(
      filename = file.path(dir_fig, out_name),
      plot = p,
      width = 10.5,
      height = 4.8
    )
  }
}

# ----------------------------------------------------------
# Nota breve de lectura económica
# ----------------------------------------------------------
note_tbl <- tibble::tibble(
  specification = paste(TAG, "p=4"),
  note = paste(
    "El reordenamiento no cambia la estimación del SVAR.",
    "Solo reorganiza la presentación de las mismas IRFs por tipo de shock.",
    "Esto facilita separar el bloque de aprendizaje inducido (v -> ga) del bloque tipo Phillips (v -> gw)",
    "y deja el shock de productividad como figura complementaria."
  )
)

readr::write_csv(
  note_tbl,
  file.path(dir_tbl, paste0("irf_reordering_note_", TAG, "_p", P_FIXED, ".csv"))
)

message("✔ 24d listo.")
message("Revisa: ", dir_fig)
message("Revisa: ", dir_tbl)
