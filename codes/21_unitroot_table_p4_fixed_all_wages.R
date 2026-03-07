# codes/21_unitroot_table_p4_fixed_all_wages.R
# ==========================================================
# 21) Tabla unificada ADF+KPSS (p=4) — 3 especificaciones salariales
# Inputs (generados por 19e p4_fixed):
#   tables/svar_core/p4_fixed/unit_root_*_p4.csv
# Outputs:
#   tables/svar_core/p4_fixed/unit_root_p4_all_specs.csv
#   tables/svar_core/p4_fixed/unit_root_p4_all_specs.tex
# ==========================================================

source(here::here("codes","00_setup.R"))
message("\n== 21) Tabla unificada ADF+KPSS (p=4) para 3 especificaciones salariales ==\n")

dir_p4 <- fs::path(CFG$dirs$out_tables, "svar_core", "p4_fixed")
stopifnot(fs::dir_exists(dir_p4))

files <- c(
  baseline = fs::path(dir_p4, "unit_root_baseline_mean_winsor_p4.csv"),
  nowinsor = fs::path(dir_p4, "unit_root_robust_mean_nowinsor_p4.csv"),
  median   = fs::path(dir_p4, "unit_root_robust_median_p4.csv")
)

missing <- files[!file.exists(files)]
if (length(missing) > 0) stop("Faltan unit_root CSV:\n- ", paste(as.character(missing), collapse = "\n- "))

read_one <- function(path, spec_label) {
  readr::read_csv(path, show_col_types = FALSE) %>%
    dplyr::mutate(
      spec = spec_label,
      # limpiar el nombre de variable (ga/gw/v) desde la columna "variable"
      var_short = dplyr::case_when(
        stringr::str_detect(variable, "^ga") ~ "ga",
        stringr::str_detect(variable, "^gw") ~ "gw",
        stringr::str_detect(variable, "^v")  ~ "v",
        TRUE ~ variable
      )
    )
}

df <- dplyr::bind_rows(
  read_one(files[["baseline"]], "Baseline (mean winsor)"),
  read_one(files[["nowinsor"]], "Robustez (mean no-winsor)"),
  read_one(files[["median"]],   "Robustez (median)")
)

# ---- Selección/orden de columnas ----
df_out <- df %>%
  dplyr::select(
    spec, var_short, T,
    ADF_type, ADF_stat, ADF_cv_10, ADF_cv_5, ADF_cv_1, ADF_reject_5pct,
    KPSS_type, KPSS_stat, KPSS_cv_10, KPSS_cv_5, KPSS_cv_1, KPSS_reject_5pct
  ) %>%
  dplyr::arrange(factor(spec, levels = c("Baseline (mean winsor)", "Robustez (mean no-winsor)", "Robustez (median)")),
                 factor(var_short, levels = c("ga","gw","v")))

# ---- Guardar CSV (trazabilidad) ----
out_csv <- fs::path(dir_p4, "unit_root_p4_all_specs.csv")
readr::write_csv(df_out, out_csv)

# ---- Crear LaTeX (booktabs) ----
fmt <- function(x, d = 3) ifelse(is.na(x), "", formatC(as.numeric(x), format = "f", digits = d))
fmt_int <- function(x) ifelse(is.na(x), "", as.character(as.integer(x)))
fmt_bool <- function(x) ifelse(is.na(x), "", ifelse(x, "Rechaza", "No rechaza"))

tex_df <- df_out %>%
  dplyr::transmute(
    Especificacion = spec,
    Variable = var_short,
    `T` = fmt_int(T),
    
    `ADF stat` = fmt(ADF_stat, 3),
    `ADF cv(5%)` = fmt(ADF_cv_5, 3),
    `ADF (5%)` = fmt_bool(ADF_reject_5pct),
    
    `KPSS stat` = fmt(KPSS_stat, 3),
    `KPSS cv(5%)` = fmt(KPSS_cv_5, 3),
    `KPSS (5%)` = fmt_bool(KPSS_reject_5pct)
  )

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

tex <- to_latex(
  tex_df,
  caption = "Tests de raíz unitaria (ADF) y estacionariedad (KPSS) para el SVAR núcleo (p=4), bajo tres definiciones alternativas de salario real.",
  label   = "tab:unitroot_p4_all_specs"
)

out_tex <- fs::path(dir_p4, "unit_root_p4_all_specs.tex")
writeLines(tex, out_tex)

message("✔ CSV:  ", as.character(out_csv))
message("✔ LaTeX:", as.character(out_tex))