# codes/19f_export_svar_p4_fixed_latex.R
# ==========================================================
# 19f) Export LaTeX (Overleaf) — Tabla resumen SVAR p4_fixed
# - Lee outputs de: tables/svar_core/p4_fixed/
# - Escribe:
#   tables/svar_core/p4_fixed/svar_p4_fixed_summary.csv
#   tables/svar_core/p4_fixed/svar_p4_fixed_summary.tex
# ==========================================================

source(here::here("codes","00_setup.R"))
message("\n== 19f) Export LaTeX: SVAR summary p4_fixed ==\n")

dir_tbl <- fs::path(CFG$dirs$out_tables, "svar_core", "p4_fixed")
stopifnot(fs::dir_exists(dir_tbl))

tags <- c("baseline_mean_winsor", "robust_mean_nowinsor", "robust_median")
p_fixed <- 4

# ---------- helpers ----------
fmt <- function(x, d = 3) ifelse(is.na(x), "", formatC(as.numeric(x), format = "f", digits = d))
fmt_p <- function(x) ifelse(is.na(x), "", formatC(as.numeric(x), format = "f", digits = 4))

read_diag_tag <- function(tg) {
  f <- fs::path(dir_tbl, paste0("var_diagnostics_", tg, "_p", p_fixed, ".csv"))
  stopifnot(file.exists(f))
  readr::read_csv(f, show_col_types = FALSE) %>%
    dplyr::mutate(
      tag = tg,
      p_used = as.integer(p_used),
      max_root_mod = as.numeric(max_root_mod),
      serial_p = as.numeric(serial_p),
      arch_p = as.numeric(arch_p)
    ) %>%
    dplyr::select(tag, p_used, stable, max_root_mod, serial_p, arch_p)
}

read_mult_tag <- function(tg) {
  f <- fs::path(dir_tbl, paste0("cum_multiplier_", tg, "_p", p_fixed, ".csv"))
  stopifnot(file.exists(f))
  readr::read_csv(f, show_col_types = FALSE) %>%
    dplyr::mutate(tag = tg) %>%
    dplyr::filter(H %in% c(4, 8, 12)) %>%
    dplyr::select(tag, H, M, M_lower, M_upper) %>%
    tidyr::pivot_wider(
      names_from = H,
      values_from = c(M, M_lower, M_upper),
      names_glue = "{.value}_H{H}"
    )
}

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

# ---------- build summary ----------
diag_all <- dplyr::bind_rows(purrr::map(tags, read_diag_tag))
mult_all <- dplyr::bind_rows(purrr::map(tags, read_mult_tag))

summary <- diag_all %>%
  dplyr::left_join(mult_all, by = "tag") %>%
  dplyr::mutate(
    especificacion = dplyr::recode(tag,
                                   "baseline_mean_winsor"   = "Baseline (media winsor)",
                                   "robust_mean_nowinsor"   = "Robustez (media sin winsor)",
                                   "robust_median"          = "Robustez (mediana)"
    ),
    estable = ifelse(stable, "Sí", "No")
  ) %>%
  dplyr::transmute(
    Especificacion = especificacion,
    `p fijo` = p_used,
    Estable = estable,
    `max |raíz|` = fmt(max_root_mod, 3),
    `p-serial` = fmt_p(serial_p),
    `p-ARCH` = fmt_p(arch_p),
    `M(4)`  = fmt(M_H4, 4),
    `M(8)`  = fmt(M_H8, 4),
    `M(12)` = fmt(M_H12, 4)
  )

# Guardar CSV summary (útil para trazabilidad)
out_csv <- fs::path(dir_tbl, "svar_p4_fixed_summary.csv")
readr::write_csv(summary, out_csv)

# Guardar TEX
tex <- to_latex(
  summary,
  caption = "SVAR núcleo (Chile 2008--2022), especificación final con p fijo = 4: diagnóstico y multiplicadores acumulados (baseline y robustez salarial).",
  label = "tab:svar_core_p4_fixed_summary"
)

out_tex <- fs::path(dir_tbl, "svar_p4_fixed_summary.tex")
writeLines(tex, out_tex)

message("✔ Guardado CSV: ", as.character(out_csv))
message("✔ Guardado LaTeX: ", as.character(out_tex))
