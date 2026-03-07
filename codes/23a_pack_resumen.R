# codes/23a_pack_resumen.R
# ==========================================================
# 23a) Pack resumen (SVAR core)
# Lee outputs de:
# - 22b seasonality_dummies_tests.csv
# - 22c trend_tests.csv
# - 22d unitroot_extended_p4_all_specs.csv
# - 19e p4_fixed var_diagnostics_*_p4.csv
# - 22a delta_v: diagnostics & cum_multiplier
# Exporta 3 tablas (CSV + TEX) en:
#   tables/svar_core/teacher_pack/
# ==========================================================

source(here::here("codes","00_setup.R"))
message("\n== 23a) Pack resumen ==\n")

# -----------------------
# Dirs (según tu setup)
# -----------------------
dir_p4      <- fs::path(CFG$dirs$out_tables, "svar_core", "p4_fixed")
dir_seas    <- fs::path(dir_p4, "seasonality")
dir_trend   <- fs::path(dir_p4, "trend")
dir_ur_ext  <- fs::path(dir_p4, "unitroot_extended")

dir_dv      <- fs::path(CFG$dirs$out_tables, "svar_core", "delta_v")
dir_out     <- fs::path(CFG$dirs$out_tables, "svar_core", "teacher_pack")

fs::dir_create(dir_out)

# -----------------------
# Inputs esperados
# -----------------------
f_seas  <- fs::path(dir_seas,  "seasonality_dummies_tests.csv")
f_trend <- fs::path(dir_trend, "trend_tests.csv")
f_ur    <- fs::path(dir_ur_ext, "unitroot_extended_p4_all_specs.csv")

f_dv_diag <- fs::path(dir_dv, "diagnostics_baseline_vs_delta_v.csv")
f_dv_mult <- fs::path(dir_dv, "cum_multiplier_baseline_vs_delta_v.csv")

needed <- c(f_seas, f_trend, f_ur, f_dv_diag, f_dv_mult)
missing <- needed[!file.exists(needed)]
if (length(missing) > 0) {
  stop("Faltan archivos necesarios:\n- ", paste(as.character(missing), collapse = "\n- "))
}

# -----------------------
# Helpers LaTeX
# -----------------------
fmt <- function(x, d = 3) ifelse(is.na(x), "", formatC(as.numeric(x), format = "f", digits = d))
fmt_p <- function(x) ifelse(is.na(x), "", formatC(as.numeric(x), format = "f", digits = 4))
fmt_bool <- function(x) ifelse(is.na(x), "", ifelse(x, "Rechaza", "No rechaza"))

to_latex <- function(df, caption, label) {
  cols <- names(df)
  align <- paste0("l", paste(rep("c", length(cols)-1), collapse = ""))
  
  header <- paste(cols, collapse = " & ")
  rows <- apply(df, 1, function(r) paste(r, collapse = " & "))
  
  paste0(
    "\\begin{table}[htbp]\n",
    "\\centering\n",
    "\\caption{", caption, "}\n",
    "\\label{", label, "}\n",
    "\\footnotesize\n",
    "\\begin{tabular}{", align, "}\n",
    "\\toprule\n",
    header, " \\\\\n",
    "\\midrule\n",
    paste0(rows, " \\\\", collapse = "\n"), "\n",
    "\\bottomrule\n",
    "\\end{tabular}\n",
    "\\end{table}\n"
  )
}

# ==========================================================
# TABLA 1) Evidencia: estacionalidad + tendencia + unit roots (ADF/KPSS)
# ==========================================================

seas <- readr::read_csv(f_seas, show_col_types = FALSE) %>%
  dplyr::rename(variable = variable) %>%
  dplyr::mutate(variable = as.character(variable))

trend <- readr::read_csv(f_trend, show_col_types = FALSE) %>%
  dplyr::mutate(variable = as.character(variable))

ur_ext <- readr::read_csv(f_ur, show_col_types = FALSE) %>%
  dplyr::mutate(
    var_short = as.character(var_short),
    spec = as.character(spec),
    ADF_type = as.character(ADF_type),
    KPSS_type = as.character(KPSS_type)
  )

# --- Resumir ADF drift vs trend (independiente de KPSS_type) ---
adf_sum <- ur_ext %>%
  dplyr::group_by(spec, var_short, ADF_type) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::select(spec, var_short, ADF_type, ADF_stat, ADF_cv_5, ADF_reject_5pct) %>%
  tidyr::pivot_wider(
    names_from = ADF_type,
    values_from = c(ADF_stat, ADF_cv_5, ADF_reject_5pct),
    names_glue = "{.value}_{ADF_type}"
  )

# --- Resumir KPSS mu vs tau (independiente de ADF_type) ---
kpss_sum <- ur_ext %>%
  dplyr::group_by(spec, var_short, KPSS_type) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::select(spec, var_short, KPSS_type, KPSS_stat, KPSS_cv_5, KPSS_reject_5pct) %>%
  tidyr::pivot_wider(
    names_from = KPSS_type,
    values_from = c(KPSS_stat, KPSS_cv_5, KPSS_reject_5pct),
    names_glue = "{.value}_{KPSS_type}"
  )

ur_summary <- adf_sum %>%
  dplyr::left_join(kpss_sum, by = c("spec","var_short"))

# merge con estacionalidad y tendencia
tab1 <- seas %>%
  dplyr::transmute(
    spec = as.character(spec),
    var_short = as.character(variable),
    T = as.integer(T),
    season_p = as.numeric(p_joint)
  ) %>%
  dplyr::left_join(
    trend %>% dplyr::transmute(
      spec = as.character(spec),
      var_short = as.character(variable),
      beta_t = as.numeric(beta_t_seas),
      p_t    = as.numeric(p_t_seas),
      r2     = as.numeric(r2_seas)
    ),
    by = c("spec","var_short")
  ) %>%
  dplyr::left_join(ur_summary, by = c("spec","var_short")) %>%
  dplyr::arrange(spec, factor(var_short, levels = c("ga","gw","v")))

# guardar CSV
out1_csv <- fs::path(dir_out, "tab1_evidence_stationarity_seasonality_trend.csv")
readr::write_csv(tab1, out1_csv)

# LaTeX (versión compacta para profe)
tab1_tex_df <- tab1 %>%
  dplyr::transmute(
    Especificacion = spec,
    Var = var_short,
    T = as.integer(T),
    `p(estac)` = fmt_p(season_p),
    `beta_t` = fmt(beta_t, 4),
    `p(trend)` = fmt_p(p_t),
    
    `ADF drift` = fmt(ADF_stat_drift, 3),
    `ADF cv5`   = fmt(ADF_cv_5_drift, 3),
    `ADF(5%)`   = fmt_bool(ADF_reject_5pct_drift),
    
    `KPSS mu`  = fmt(KPSS_stat_mu, 3),
    `KPSS cv5` = fmt(KPSS_cv_5_mu, 3),
    `KPSS(5%)` = fmt_bool(KPSS_reject_5pct_mu)
  )

out1_tex <- fs::path(dir_out, "tab1_evidence_stationarity_seasonality_trend.tex")
writeLines(
  to_latex(
    tab1_tex_df,
    caption = "Evidencia de estacionalidad, tendencia determinística y estacionariedad (ADF/KPSS) para (ga, gw, v), p=4.",
    label = "tab:evidence_stationarity_p4"
  ),
  out1_tex
)

message("✔ TABLA 1 CSV:  ", as.character(out1_csv))
message("✔ TABLA 1 LaTeX:", as.character(out1_tex))

# ==========================================================
# TABLA 2) Diagnósticos VAR/SVAR (p4_fixed) por especificación salarial
# ==========================================================

diag_files <- c(
  fs::path(dir_p4, "var_diagnostics_baseline_mean_winsor_p4.csv"),
  fs::path(dir_p4, "var_diagnostics_robust_mean_nowinsor_p4.csv"),
  fs::path(dir_p4, "var_diagnostics_robust_median_p4.csv")
)
missing2 <- diag_files[!file.exists(diag_files)]
if (length(missing2) > 0) stop("Faltan var_diagnostics en p4_fixed:\n- ", paste(as.character(missing2), collapse="\n- "))

tab2_raw <- purrr::map_dfr(diag_files, \(f) readr::read_csv(f, show_col_types = FALSE))

# blindaje: si no existen columnas del SVAR, crearlas como NA
if (!"svar_B" %in% names(tab2_raw))      tab2_raw$svar_B <- NA_character_
if (!"svar_method" %in% names(tab2_raw)) tab2_raw$svar_method <- NA_character_

tab2 <- tab2_raw %>%
  dplyr::mutate(
    tag = as.character(tag),
    p_used = as.integer(p_used),
    max_root_mod = as.numeric(max_root_mod),
    serial_p = as.numeric(serial_p),
    arch_p = as.numeric(arch_p)
  ) %>%
  dplyr::select(tag, p_used, stable, max_root_mod, serial_p, arch_p, svar_B, svar_method) %>%
  dplyr::arrange(tag)

out2_csv <- fs::path(dir_out, "tab2_var_diagnostics_p4_fixed.csv")
readr::write_csv(tab2, out2_csv)

tab2_tex_df <- tab2 %>%
  dplyr::transmute(
    Especificacion = tag,
    p = p_used,
    Estable = ifelse(stable, "Sí", "No"),
    `max|raiz|` = fmt(max_root_mod, 3),
    `p-serial` = fmt_p(serial_p),
    `p-ARCH`   = fmt_p(arch_p),
    B = svar_B,
    Metodo = svar_method
  )

out2_tex <- fs::path(dir_out, "tab2_var_diagnostics_p4_fixed.tex")
writeLines(
  to_latex(
    tab2_tex_df,
    caption = "Diagnósticos del VAR/SVAR reducido (p=4) por especificación salarial.",
    label = "tab:vardiagnostics_p4"
  ),
  out2_tex
)

message("✔ TABLA 2 CSV:  ", as.character(out2_csv))
message("✔ TABLA 2 LaTeX:", as.character(out2_tex))

# ==========================================================
# TABLA 3) Robustez Δv: diagnósticos + M(H)
# ==========================================================

dv_diag <- readr::read_csv(f_dv_diag, show_col_types = FALSE) %>%
  dplyr::mutate(
    sample = as.character(sample),
    p_used = as.integer(p_used),
    max_root_mod = as.numeric(max_root_mod),
    serial_p = as.numeric(serial_p),
    arch_p = as.numeric(arch_p)
  )

# ✅ Blindaje: si no existen, créalas para que no rompa el select()
if (!"svar_B" %in% names(dv_diag))      dv_diag$svar_B <- NA_character_
if (!"svar_method" %in% names(dv_diag)) dv_diag$svar_method <- NA_character_

dv_diag <- dv_diag %>%
  dplyr::select(sample, p_used, stable, max_root_mod, serial_p, arch_p, svar_B, svar_method)

dv_mult <- readr::read_csv(f_dv_mult, show_col_types = FALSE) %>%
  dplyr::mutate(sample = as.character(sample)) %>%
  dplyr::filter(H %in% c(4,8,12)) %>%
  dplyr::select(sample, H, M, M_lower, M_upper) %>%
  tidyr::pivot_wider(
    names_from = H,
    values_from = c(M, M_lower, M_upper),
    names_glue = "{.value}_H{H}"
  )

tab3 <- dv_diag %>%
  dplyr::left_join(dv_mult, by = "sample") %>%
  dplyr::arrange(sample)

out3_csv <- fs::path(dir_out, "tab3_delta_v_robustness.csv")
readr::write_csv(tab3, out3_csv)

tab3_tex_df <- tab3 %>%
  dplyr::transmute(
    Especificacion = sample,
    p = p_used,
    Estable = ifelse(stable, "Sí", "No"),
    `max|raiz|` = fmt(max_root_mod, 3),
    `p-serial` = fmt_p(serial_p),
    `M(4)`  = fmt(M_H4, 4),
    `M(8)`  = fmt(M_H8, 4),
    `M(12)` = fmt(M_H12, 4)
  )

out3_tex <- fs::path(dir_out, "tab3_delta_v_robustness.tex")
writeLines(
  to_latex(
    tab3_tex_df,
    caption = "Robustez: reemplazo de v por Δv (p=4). Diagnósticos y multiplicadores acumulados.",
    label = "tab:delta_v_robustness"
  ),
  out3_tex
)

message("✔ TABLA 3 CSV:  ", as.character(out3_csv))
message("✔ TABLA 3 LaTeX:", as.character(out3_tex))

message("\n✔ 23a listo: pack en ", as.character(dir_out))