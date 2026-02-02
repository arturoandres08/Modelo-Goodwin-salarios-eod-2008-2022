# codes/00_setup.R
# ===============================================================
# 00) Setup global del proyecto (reproducible)
# - Paquetes
# - Config centralizada (CFG): rutas + parámetros
# - Helpers I/O (save_rds, load_rds, save_table_csv, save_plot_file)
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

# ---- CFG: rutas y parámetros del proyecto ----
# Respetamos tu estructura actual recomendada por tu profe:
# data/raw/{eod,ipc,imacec}, data/interim, data/processed, data/validation
# tables/, figures/, auditoria_variables/, logs/
CFG <- list(
  project = list(
    name = "Trabajo_R_Tesis",
    timezone = "America/Santiago"
  ),
  
  dirs = list(
    # data
    data_raw    = here("data", "raw"),
    data_eod    = here("data", "raw", "eod"),
    data_ipc    = here("data", "raw", "ipc"),
    data_imacec = here("data", "raw", "imacec"),
    
    # pipeline
    interim     = here("data", "interim"),
    processed   = here("data", "processed"),
    validation  = here("data", "validation"),
    
    # outputs finales (paper)
    out_tables  = here("tables"),
    out_figures = here("figures"),
    
    # auditoría/diagnóstico
    aud_tab     = here("auditoria_variables", "tablas"),
    aud_fig     = here("auditoria_variables", "graficos"),
    
    # logs
    logs        = here("logs")
  ),
  
  files = list(
    ipc_excel    = here("data", "raw", "ipc", "PEM_VAR_IPC_2018_HIST.xlsx"),
    imacec_excel = here("data", "raw", "imacec", "CCNN2018_IMACEC_01_A.xlsx"),
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

fs::dir_create(CFG$dirs$logs)

# ---- Helpers I/O ----
save_rds <- function(x, path) {
  fs::dir_create(fs::path_dir(path))
  saveRDS(x, path)
  invisible(path)
}

load_rds <- function(path) readRDS(path)

save_table_csv <- function(df, name, dir = CFG$dirs$out_tables) {
  fs::dir_create(dir)
  path <- fs::path(dir, paste0(name, ".csv"))
  readr::write_csv(df, path)
  invisible(path)
}

save_plot_file <- function(p, name, dir = CFG$dirs$out_figures,
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

# ---- Paths intermedios/procesados (centralizados) ----
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

# ---- Helpers trimestre ----
q_num <- function(tri) match(toupper(tri), c("T1","T2","T3","T4"))

tri_from_month <- function(mm) {
  mm <- suppressWarnings(as.integer(mm))
  ifelse(is.na(mm), NA_character_, paste0("T", ceiling(mm / 3)))
}

# ---- Backward compatibility (por si algún script viejo usa DIR_*) ----
DIR_RAW_EOD    <- CFG$dirs$data_eod
DIR_RAW_IPC    <- CFG$dirs$data_ipc
DIR_RAW_IMACEC <- CFG$dirs$data_imacec
DIR_PROCESSED  <- CFG$dirs$processed
DIR_INTERIM    <- CFG$dirs$interim
DIR_VALIDATION <- CFG$dirs$validation
DIR_AUD_TAB    <- CFG$dirs$aud_tab
DIR_AUD_FIG    <- CFG$dirs$aud_fig
