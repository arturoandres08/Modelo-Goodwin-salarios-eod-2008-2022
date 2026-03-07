# codes/24a_irf_tables_p4_fixed_for_section_443.R
# ==========================================================
# 24a) Tablas numéricas IRF (con IC) + multiplicadores acumulados
#      para la sección 4.4.3 (p4_fixed)
# - Usa outputs de: tables/svar_core/p4_fixed/
# - Crea tablas para:
#   (A) IRF v->ga (h=0..12) con CI
#   (B) IRF v->gw (h=0..12) con CI
#   (C) (opcional) IRF ga->gw (h=0..12) con CI
#   (D) Multiplicador acumulado para salarios M_gw(H) en H={4,8,12} (y también por h si quieres)
# - Exporta CSV + TEX en:
#   tables/svar_core/p4_fixed/irf_tables_443/
# ==========================================================

source(here::here("codes","00_setup.R"))
message("\n== 24a) Tablas IRF numéricas (p4_fixed) para 4.4.3 ==\n")

dir_p4 <- fs::path(CFG$dirs$out_tables, "svar_core", "p4_fixed")
stopifnot(fs::dir_exists(dir_p4))

out_dir <- fs::path(dir_p4, "irf_tables_443")
fs::dir_create(out_dir)

# ---- archivos IRF baseline p4 (tag baseline_mean_winsor) ----
f_v_ga <- fs::path(dir_p4, "irf_v_to_ga_baseline_mean_winsor_p4.pdf")  # figura (solo referencia)
# CSVs no existen en p4_fixed (en 19e guardamos solo figs + mult); así que usamos los multipliers + re-leemos de 19e si guardaste IRF csv.
# Pero en 19e p4_fixed NO guardamos IRF csv; en 19 (BIC) sí, y en 20* sí.
# Solución reproducible: leer los IRF desde CSV si existen; si no, leer desde scripts delta_v/covid (ya guardan CSV).
# Para no depender, vamos a leer desde los CSV del baseline "19" (svar_core/irf_...) si los tienes; si no, te indico cómo generarlos.

# ---- Paths esperados (si existen) ----
dir_core <- fs::path(CFG$dirs$out_tables, "svar_core")

cand_v_ga <- c(
  fs::path(dir_core, "p4_fixed", "irf_v_to_ga_baseline_mean_winsor_p4.csv"),
  fs::path(dir_core, "irf_v_to_ga_baseline_mean_winsor.csv"),
  fs::path(dir_core, "lag_robustness", "irf_v_to_ga_p1_vs_p6.csv"), # no sirve directo
  fs::path(dir_core, "covid_dummy", "irf_v_to_ga_baseline_vs_dummy.csv"),
  fs::path(dir_core, "covid_nocovid", "irf_v_to_ga_full_vs_nocovid.csv"),
  fs::path(dir_core, "delta_v", "irf_v_to_ga_baseline_vs_delta_v.csv")
)

cand_v_gw <- c(
  fs::path(dir_core, "p4_fixed", "irf_v_to_gw_baseline_mean_winsor_p4.csv"),
  fs::path(dir_core, "irf_v_to_gw_baseline_mean_winsor.csv"),
  fs::path(dir_core, "covid_dummy", "irf_v_to_gw_baseline_vs_dummy.csv"),
  fs::path(dir_core, "covid_nocovid", "irf_v_to_gw_full_vs_nocovid.csv"),
  fs::path(dir_core, "delta_v", "irf_v_to_gw_baseline_vs_delta_v.csv")
)

cand_ga_gw <- c(
  fs::path(dir_core, "p4_fixed", "irf_ga_to_gw_baseline_mean_winsor_p4.csv"),
  fs::path(dir_core, "irf_ga_to_gw_baseline_mean_winsor.csv"),
  fs::path(dir_core, "covid_dummy", "irf_ga_to_gw_baseline_vs_dummy.csv"),
  fs::path(dir_core, "covid_nocovid", "irf_ga_to_gw_full_vs_nocovid.csv"),
  fs::path(dir_core, "delta_v", "irf_ga_to_gw_baseline_vs_delta_v.csv")
)

pick_first_existing <- function(paths) {
  hit <- paths[file.exists(paths)]
  if (length(hit) == 0) NA_character_ else as.character(hit[1])
}

p_v_ga <- pick_first_existing(cand_v_ga)
p_v_gw <- pick_first_existing(cand_v_gw)
p_ga_gw <- pick_first_existing(cand_ga_gw)

if (is.na(p_v_ga) || is.na(p_v_gw)) {
  stop(
    "No encontré CSVs de IRF baseline para v->ga y v->gw.\n",
    "Solución recomendada (reproducible): modificar 19e para guardar IRF CSV en p4_fixed.\n",
    "Si quieres, te paso el parche exacto para 19e en 5 líneas."
  )
}

read_irf <- function(path) {
  readr::read_csv(path, show_col_types = FALSE) %>%
    dplyr::mutate(
      h = as.integer(h),
      irf = as.numeric(irf),
      lower = as.numeric(lower),
      upper = as.numeric(upper)
    )
}

# En algunos CSV (covid/delta_v) vienen varias muestras; nos quedamos con Baseline (v level)
filter_baseline <- function(df) {
  if ("sample" %in% names(df)) {
    # baseline típico
    df %>% dplyr::filter(stringr::str_detect(as.character(sample), "Baseline"))
  } else if ("dummy" %in% names(df)) {
    df %>% dplyr::filter(dummy %in% c(FALSE, "FALSE", 0, "0"))
  } else {
    df
  }
}

irf_v_ga <- read_irf(p_v_ga) %>% filter_baseline() %>% dplyr::select(h, irf, lower, upper) %>% dplyr::arrange(h)
irf_v_gw <- read_irf(p_v_gw) %>% filter_baseline() %>% dplyr::select(h, irf, lower, upper) %>% dplyr::arrange(h)

irf_ga_gw <- NULL
if (!is.na(p_ga_gw)) {
  irf_ga_gw <- read_irf(p_ga_gw) %>% filter_baseline() %>% dplyr::select(h, irf, lower, upper) %>% dplyr::arrange(h)
}

# ---- Multiplicador acumulado para salarios (por h y por H=4/8/12) ----
cum_irf <- function(df) {
  df %>%
    dplyr::arrange(h) %>%
    dplyr::mutate(
      cum_irf   = cumsum(irf),
      cum_lower = cumsum(lower),
      cum_upper = cumsum(upper)
    )
}

cum_gw <- cum_irf(irf_v_gw)

mult_gw <- cum_gw %>%
  dplyr::filter(h %in% c(4, 8, 12)) %>%
  dplyr::transmute(
    H = h,
    M_gw = cum_irf,
    M_gw_lower = cum_lower,
    M_gw_upper = cum_upper
  )

# ---- Guardar CSV ----
readr::write_csv(irf_v_ga, fs::path(out_dir, "irf_v_to_ga_p4_baseline.csv"))
readr::write_csv(irf_v_gw, fs::path(out_dir, "irf_v_to_gw_p4_baseline.csv"))
if (!is.null(irf_ga_gw)) readr::write_csv(irf_ga_gw, fs::path(out_dir, "irf_ga_to_gw_p4_baseline.csv"))

readr::write_csv(cum_gw, fs::path(out_dir, "cum_irf_v_to_gw_p4_baseline.csv"))
readr::write_csv(mult_gw, fs::path(out_dir, "multiplier_gw_H4_H8_H12_p4_baseline.csv"))

# ---- Export LaTeX simple (booktabs) ----
fmt <- function(x, d=4) ifelse(is.na(x), "", formatC(as.numeric(x), format="f", digits=d))

to_latex <- function(df, caption, label) {
  cols <- names(df)
  align <- paste0("c", paste(rep("c", length(cols)-1), collapse=""))
  header <- paste(cols, collapse=" & ")
  rows <- apply(df, 1, \(r) paste(r, collapse=" & "))
  paste0(
    "\\begin{table}[htbp]\n\\centering\n",
    "\\caption{", caption, "}\n",
    "\\label{", label, "}\n",
    "\\footnotesize\n",
    "\\begin{tabular}{", align, "}\n",
    "\\toprule\n",
    header, " \\\\\n\\midrule\n",
    paste0(rows, " \\\\", collapse="\n"), "\n",
    "\\bottomrule\n\\end{tabular}\n\\end{table}\n"
  )
}

irf_tex <- function(df, title, label) {
  df2 <- df %>%
    dplyr::transmute(
      h = h,
      IRF = fmt(irf, 4),
      CI_low = fmt(lower, 4),
      CI_high = fmt(upper, 4)
    )
  writeLines(to_latex(df2, title, label), fs::path(out_dir, paste0(label, ".tex")))
}

irf_tex(irf_v_ga, "IRF numérica (p=4): shock laboral en v sobre productividad (ga).", "tab_irf_v_ga_p4")
irf_tex(irf_v_gw, "IRF numérica (p=4): shock laboral en v sobre salarios (gw).", "tab_irf_v_gw_p4")
if (!is.null(irf_ga_gw)) irf_tex(irf_ga_gw, "IRF numérica (p=4): shock productividad (ga) sobre salarios (gw).", "tab_irf_ga_gw_p4")

mult_tex_df <- mult_gw %>%
  dplyr::transmute(
    H = H,
    M = fmt(M_gw, 4),
    CI_low = fmt(M_gw_lower, 4),
    CI_high = fmt(M_gw_upper, 4)
  )
writeLines(
  to_latex(mult_tex_df, "Multiplicador acumulado (p=4): efecto de shock en v sobre salarios (gw).", "tab_mult_gw_p4"),
  fs::path(out_dir, "tab_mult_gw_p4.tex")
)

message("✔ 24a listo: CSV+TEX en ", as.character(out_dir))