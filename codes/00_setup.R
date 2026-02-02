# codes/00_setup.R
# ===============================================================
# 00) Setup global del proyecto (reproducible)
# - Paquetes
# - Config centralizada (CFG): rutas + parámetros
# - Helpers I/O (save_rds, load_rds, save_*_csv, save_*_plot_file)
# ===============================================================

# ---- Paquetes base ----
pkgs <- c(
  "tidyverse", "dplyr", "purrr", "stringr", "readr", "janitor",
  "haven", "readxl", "lubridate",
  "ggplot2", "scales",
  "fs", "here", "tidyr"
)

to_install <- pkgs[!vapply(pkgs, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)]
if (length(to_install) > 0) install.packages(to_install, repos = "https://cloud.r-project.org")
invisible(lapply(pkgs, library, character.only = TRUE))

# ---- Root explícito (blindaje) ----
ROOT <- here::here()

# ---- CFG: rutas y parámetros del proyecto ----
# Estructura reproducible tipo magíster:
# data/raw/{eod,ipc,imacec}, data/interim, data/processed, data/validation
# outputs finales: tables/, figures/
# auditoría: auditoria_variables/{tables,figures,validation}
CFG <- list(
  project = list(
    name = "Trabajo_R_Tesis",
    timezone = "America/Santiago",
    root = ROOT
  ),
  
  dirs = list(
    # data
    data_raw    = here::here("data", "raw"),
    data_eod    = here::here("data", "raw", "eod"),
    data_ipc    = here::here("data", "raw", "ipc"),
    data_imacec = here::here("data", "raw", "imacec"),
    
    # pipeline
    interim     = here::here("data", "interim"),
    processed   = here::here("data", "processed"),
    validation  = here::here("data", "validation"),
    
    # outputs finales (paper)
    out_tables  = here::here("tables"),
    out_figures = here::here("figures"),
    
    # auditoría / diagnóstico (HUMANO)
    aud_tab     = here::here("auditoria_variables", "tables"),
    aud_fig     = here::here("auditoria_variables", "figures"),
    aud_val     = here::here("auditoria_variables", "validation"),
    
    # logs
    logs        = here::here("logs")
  ),
  
  files = list(
    ipc_excel    = here::here("data", "raw", "ipc", "PEM_VAR_IPC_2018_HIST.xlsx"),
    imacec_excel = here::here("data", "raw", "imacec", "CCNN2018_IMACEC_01_A.xlsx"),
    imacec_sheet = "Cuadro"
  ),
  
  params = list(
    year_min = 2008L,
    year_max = 2022L,
    
    income_cap_nominal = 10000000, # tope 10 millones
    winsor_p = 0.995,              # winsor p99.5
    
    base_year = 2018L,
    base_month_ipc = 12L
  )
)

# ---- (Opcional) fijar TZ para consistencia ----
Sys.setenv(TZ = CFG$project$timezone)

# ---- Crear carpetas clave (si no existen) ----
fs::dir_create(CFG$dirs$data_raw)
fs::dir_create(CFG$dirs$data_eod)
fs::dir_create(CFG$dirs$data_ipc)
fs::dir_create(CFG$dirs$data_imacec)

fs::dir_create(CFG$dirs$interim)
fs::dir_create(CFG$dirs$processed)
fs::dir_create(CFG$dirs$validation)

fs::dir_create(CFG$dirs$out_tables)
fs::dir_create(CFG$dirs$out_figures)

fs::dir_create(CFG$dirs$aud_tab)
fs::dir_create(CFG$dirs$aud_fig)
fs::dir_create(CFG$dirs$aud_val)

fs::dir_create(CFG$dirs$logs)

# ---- Checks tempranos (recomendados) ----
# Si quieres permitir correr sin excels (p.ej. en otro PC), comenta estos 2 stopifnot(file.exists...)
stopifnot(fs::dir_exists(CFG$dirs$data_eod))
stopifnot(fs::dir_exists(CFG$dirs$data_ipc))
stopifnot(fs::dir_exists(CFG$dirs$data_imacec))
stopifnot(file.exists(CFG$files$ipc_excel))
stopifnot(file.exists(CFG$files$imacec_excel))

# ===============================================================
# Helpers I/O
# ===============================================================

save_rds <- function(x, path) {
  fs::dir_create(fs::path_dir(path))
  saveRDS(x, path)
  invisible(path)
}

load_rds <- function(path) readRDS(path)

# ---- Outputs finales (paper) ----
save_paper_csv <- function(df, name, dir = CFG$dirs$out_tables) {
  fs::dir_create(dir)
  path <- fs::path(dir, paste0(name, ".csv"))
  readr::write_csv(df, path)
  invisible(path)
}

save_paper_plot_file <- function(p, name, dir = CFG$dirs$out_figures,
                                 width = 10, height = 5, dpi = 300,
                                 device = c("png", "pdf")) {
  device <- match.arg(device)
  fs::dir_create(dir)
  path <- fs::path(dir, paste0(name, ".", device))
  ggplot2::ggsave(
    filename = path, plot = p,
    width = width, height = height,
    dpi = dpi, units = "in", device = device
  )
  invisible(path)
}

# ---- Auditoría / diagnóstico (separado, “humano”) ----
save_audit_csv <- function(df, name, dir = CFG$dirs$aud_tab) {
  fs::dir_create(dir)
  path <- fs::path(dir, paste0(name, ".csv"))
  readr::write_csv(df, path)
  invisible(path)
}

save_audit_validation_csv <- function(df, name, dir = CFG$dirs$aud_val) {
  fs::dir_create(dir)
  path <- fs::path(dir, paste0(name, ".csv"))
  readr::write_csv(df, path)
  invisible(path)
}

save_audit_plot_file <- function(p, name, dir = CFG$dirs$aud_fig,
                                 width = 10, height = 5, dpi = 300,
                                 device = c("png", "pdf")) {
  device <- match.arg(device)
  fs::dir_create(dir)
  path <- fs::path(dir, paste0(name, ".", device))
  ggplot2::ggsave(
    filename = path, plot = p,
    width = width, height = height,
    dpi = dpi, units = "in", device = device
  )
  invisible(path)
}

# ---- Mantener compatibilidad con tus scripts actuales ----
# Tus scripts hoy usan: save_table_csv() y save_plot_file()
save_table_csv <- save_paper_csv
save_plot_file <- save_paper_plot_file

# ===============================================================
# Paths intermedios/procesados (centralizados)
# ===============================================================
CFG$paths <- list(
  # EOD salarios (interim)
  eod_individual_raw      = fs::path(CFG$dirs$interim, "eod_extract_individual_raw_like.rds"),
  eod_calendar_normalized = fs::path(CFG$dirs$interim, "eod_individual_calendar_normalized.rds"),
  eod_clean_2008_2022     = fs::path(CFG$dirs$interim, "eod_individual_2008_2022_clean.rds"),
  eod_winsor_2008_2022    = fs::path(CFG$dirs$interim, "eod_individual_2008_2022_winsor.rds"),
  
  # salarios (processed)
  nominal_quarterly       = fs::path(CFG$dirs$processed, "nominal_quarterly_2008_2022.rds"),
  ipc_monthly             = fs::path(CFG$dirs$processed, "ipc_monthly_base2018_100.rds"),
  real_quarterly          = fs::path(CFG$dirs$processed, "real_quarterly_2008_2022.rds"),
  
  # desempleo (interim/processed)
  unemp_individual_2008_2022 = fs::path(CFG$dirs$interim, "eod_unemployment_individual_2008_2022.rds"),
  unemp_quarterly            = fs::path(CFG$dirs$processed, "unemployment_quarterly_2008_2022.rds"),
  
  # IMACEC / empleo / productividad (processed)
  imacec_quarterly         = fs::path(CFG$dirs$processed, "imacec_quarterly_2008_2022_base2018_100.rds"),
  employment_quarterly     = fs::path(CFG$dirs$processed, "employment_quarterly_2008_2022_base2018_100.rds"),
  prod_total_quarterly     = fs::path(CFG$dirs$processed, "productivity_quarterly_total_2008_2022_base2018_100.rds"),
  prod_nonmining_quarterly = fs::path(CFG$dirs$processed, "productivity_quarterly_nonmining_2008_2022_base2018_100.rds"),
  
  # CV adjusted (processed)
  cv_adjusted_sinwinsor    = fs::path(CFG$dirs$processed, "wages_real_quarterly_sinwinsor_cv_2008_2022.rds"),
  cv_adjusted_winsor       = fs::path(CFG$dirs$processed, "wages_real_quarterly_winsor_cv_2008_2022.rds")
)

# ===============================================================
# Helpers trimestre
# ===============================================================
q_num <- function(tri) match(toupper(tri), c("T1", "T2", "T3", "T4"))

tri_from_month <- function(mm) {
  mm <- suppressWarnings(as.integer(mm))
  ifelse(is.na(mm), NA_character_, paste0("T", ceiling(mm / 3)))
}

# ===============================================================
# Backward compatibility (DIR_*)
# ===============================================================
DIR_RAW_EOD    <- CFG$dirs$data_eod
DIR_RAW_IPC    <- CFG$dirs$data_ipc
DIR_RAW_IMACEC <- CFG$dirs$data_imacec
DIR_PROCESSED  <- CFG$dirs$processed
DIR_INTERIM    <- CFG$dirs$interim
DIR_VALIDATION <- CFG$dirs$validation
DIR_AUD_TAB    <- CFG$dirs$aud_tab
DIR_AUD_FIG    <- CFG$dirs$aud_fig


# ---- Backward compatibility: objetos antiguos (CFGpaths/CFGdirs/CFGparams/CFGfiles) ----
CFGpaths  <- CFG$paths
CFGdirs   <- CFG$dirs
CFGparams <- CFG$params
CFGfiles  <- CFG$files

