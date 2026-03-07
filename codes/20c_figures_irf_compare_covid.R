# codes/20c_figures_irf_compare_covid.R
# ==========================================================
# 20c) Figuras comparativas IRF: Baseline vs Dummy COVID vs noCOVID
# - Inputs (generados por 20a y 20b):
#   tables/svar_core/covid_dummy/   irf_*_baseline_vs_dummy.csv
#   tables/svar_core/covid_nocovid/ irf_*_full_vs_nocovid.csv
# - Outputs:
#   figures/svar_core/covid_robust/ 3 PDFs + 3 PNGs
# ==========================================================

source(here::here("codes","00_setup.R"))
message("\n== 20c) IRFs comparativas: Baseline vs Dummy COVID vs noCOVID ==\n")

# ---------------------------
# Directorios de input/output según setup
# ---------------------------
dir_dummy   <- fs::path(CFG$dirs$out_tables,  "svar_core", "covid_dummy")
dir_nocovid <- fs::path(CFG$dirs$out_tables,  "svar_core", "covid_nocovid")
out_dir     <- fs::path(CFG$dirs$out_figures, "svar_core", "covid_robust")

fs::dir_create(out_dir)

# ---------------------------
# Paths esperados (6 CSV)
# ---------------------------
p_v_ga_dummy  <- fs::path(dir_dummy,   "irf_v_to_ga_baseline_vs_dummy.csv")
p_v_gw_dummy  <- fs::path(dir_dummy,   "irf_v_to_gw_baseline_vs_dummy.csv")
p_ga_gw_dummy <- fs::path(dir_dummy,   "irf_ga_to_gw_baseline_vs_dummy.csv")

p_v_ga_nocov  <- fs::path(dir_nocovid, "irf_v_to_ga_full_vs_nocovid.csv")
p_v_gw_nocov  <- fs::path(dir_nocovid, "irf_v_to_gw_full_vs_nocovid.csv")
p_ga_gw_nocov <- fs::path(dir_nocovid, "irf_ga_to_gw_full_vs_nocovid.csv")

needed <- c(p_v_ga_dummy, p_v_gw_dummy, p_ga_gw_dummy,
            p_v_ga_nocov, p_v_gw_nocov, p_ga_gw_nocov)

missing <- needed[!file.exists(needed)]
if (length(missing) > 0) {
  stop("Faltan CSVs de IRF. No encontré:\n- ",
       paste(as.character(missing), collapse = "\n- "))
}

# ---------------------------
# Helpers
# ---------------------------
read_irf <- function(path) {
  df <- readr::read_csv(path, show_col_types = FALSE)
  
  # columnas mínimas
  req <- c("h","irf","lower","upper")
  if (!all(req %in% names(df))) {
    stop("El archivo no tiene columnas mínimas (h, irf, lower, upper): ", as.character(path))
  }
  
  df %>%
    dplyr::mutate(
      h = as.integer(h),
      irf = as.numeric(irf),
      lower = as.numeric(lower),
      upper = as.numeric(upper)
    )
}

# Baseline vs Dummy: intenta usar columna dummy; si no existe, parsea 'sample'
prep_dummy <- function(df) {
  if ("dummy" %in% names(df)) {
    df %>%
      dplyr::mutate(
        spec = dplyr::if_else(dummy %in% c(TRUE, "TRUE", 1, "1"), "Dummy COVID", "Baseline")
      )
  } else if ("sample" %in% names(df)) {
    df %>%
      dplyr::mutate(
        sample = as.character(sample),
        spec = dplyr::if_else(stringr::str_detect(sample, "Dummy|dummy|COVID"), "Dummy COVID", "Baseline")
      )
  } else {
    stop("No puedo construir 'spec' en el archivo baseline_vs_dummy: falta 'dummy' o 'sample'.")
  }
}

# Full vs noCOVID: nos quedamos SOLO con la fila noCOVID
prep_nocovid_only <- function(df) {
  if (!"sample" %in% names(df)) stop("El archivo full_vs_nocovid no tiene columna 'sample'.")
  df %>%
    dplyr::mutate(sample = as.character(sample)) %>%
    dplyr::filter(stringr::str_detect(sample, "noCOVID|nocovid|noCovid")) %>%
    dplyr::mutate(spec = "noCOVID")
}

# Chequeo: mismos horizontes para cada spec
check_horizons <- function(df_all, label) {
  hh <- df_all %>%
    dplyr::group_by(spec) %>%
    dplyr::summarise(
      h_min = min(h, na.rm = TRUE),
      h_max = max(h, na.rm = TRUE),
      n_h = dplyr::n_distinct(h),
      .groups = "drop"
    )
  # Solo warning (no stop) por si una serie viene con distinto n.ahead
  if (nrow(unique(hh[,c("h_min","h_max","n_h")])) > 1) {
    warning("Horizontes distintos entre specs en ", label, ":\n",
            paste(capture.output(print(hh)), collapse = "\n"))
  }
  invisible(hh)
}

plot_irf_compare <- function(df, title, ylab, out_name_base) {
  df <- df %>%
    dplyr::mutate(
      spec = factor(spec, levels = c("Baseline", "Dummy COVID", "noCOVID"))
    ) %>%
    dplyr::arrange(spec, h)
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = h, y = irf, color = spec, fill = spec)) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.35) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), alpha = 0.15, linewidth = 0) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::scale_x_continuous(breaks = seq(0, 12, 2)) +
    ggplot2::labs(title = title, x = "Horizonte (trimestres)", y = ylab, color = NULL, fill = NULL) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(legend.position = "bottom")
  
  ggplot2::ggsave(filename = file.path(as.character(out_dir), paste0(out_name_base, ".pdf")),
                  plot = p, width = 8, height = 4.8)
  ggplot2::ggsave(filename = file.path(as.character(out_dir), paste0(out_name_base, ".png")),
                  plot = p, width = 8, height = 4.8, dpi = 300)
  
  invisible(p)
}

# Construye dataset 3 vías (Baseline + Dummy + noCOVID)
build_threeway <- function(path_dummy, path_nocovid, label) {
  df_dummy <- read_irf(path_dummy) %>%
    prep_dummy() %>%
    dplyr::select(h, irf, lower, upper, spec)
  
  df_nocov <- read_irf(path_nocovid) %>%
    prep_nocovid_only() %>%
    dplyr::select(h, irf, lower, upper, spec)
  
  df_all <- dplyr::bind_rows(df_dummy, df_nocov) %>%
    dplyr::mutate(spec = factor(spec, levels = c("Baseline", "Dummy COVID", "noCOVID")))
  
  check_horizons(df_all, label)
  df_all
}

# ---------------------------
# Build datasets (3 IRFs)
# ---------------------------
df_v_ga  <- build_threeway(p_v_ga_dummy,  p_v_ga_nocov,  "v->ga")
df_v_gw  <- build_threeway(p_v_gw_dummy,  p_v_gw_nocov,  "v->gw")
df_ga_gw <- build_threeway(p_ga_gw_dummy, p_ga_gw_nocov, "ga->gw")

# ---------------------------
# Plot (3 figuras)
# Nota: uso "->" para evitar warnings de encoding con "→"
# ---------------------------
plot_irf_compare(
  df_v_ga,
  "IRF: shock empleo (v) -> productividad (ga) | Baseline vs Dummy COVID vs noCOVID (p=4)",
  "Respuesta (ga)",
  "IRF_compare_v_to_ga_p4"
)

plot_irf_compare(
  df_v_gw,
  "IRF: shock empleo (v) -> salarios (gw) | Baseline vs Dummy COVID vs noCOVID (p=4)",
  "Respuesta (gw)",
  "IRF_compare_v_to_gw_p4"
)

plot_irf_compare(
  df_ga_gw,
  "IRF: shock productividad (ga) -> salarios (gw) | Baseline vs Dummy COVID vs noCOVID (p=4)",
  "Respuesta (gw)",
  "IRF_compare_ga_to_gw_p4"
)

message("✔ Listo: revisa ", normalizePath(as.character(out_dir)))