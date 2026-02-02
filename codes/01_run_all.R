# codes/01_run_all.R
source(here::here("codes","00_setup.R"))

# ------------------------------------------------------------
# 0) Logger simple (archivo + consola)
# ------------------------------------------------------------
fs::dir_create(CFG$dirs$logs)

log_file <- fs::path(
  CFG$dirs$logs,
  paste0("run_all_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log")
)

log_msg <- function(...) {
  msg <- paste0(
    "[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ",
    paste0(..., collapse = "")
  )
  cat(msg, "\n")
  cat(msg, "\n", file = log_file, append = TRUE)
}

# Guarda sessionInfo a archivo
save_session_info <- function(path_txt) {
  si <- capture.output(sessionInfo())
  writeLines(si, con = path_txt)
  invisible(path_txt)
}

# Busca el script con nombre A; si no existe, intenta con nombre B
resolve_step <- function(primary, fallback = NULL) {
  p1 <- here("codes", primary)
  if (file.exists(p1)) return(p1)
  
  if (!is.null(fallback) && !is.na(fallback) && nzchar(fallback)) {
    p2 <- here("codes", fallback)
    if (file.exists(p2)) return(p2)
  }
  
  stop(
    "No se encontró el script: ", primary,
    if (!is.null(fallback) && !is.na(fallback) && nzchar(fallback))
      paste0(" (ni el fallback: ", fallback, ")") else ""
  )
}

run_step <- function(path) {
  stopifnot(file.exists(path))
  log_msg("▶ Running: ", path)
  t0 <- Sys.time()
  
  ok <- tryCatch(
    {
      # Capturar warnings también en el log
      withCallingHandlers(
        {
          source(path, local = FALSE)  # local=FALSE para pipeline reproducible
        },
        warning = function(w) {
          log_msg("⚠ WARNING in ", path, " :: ", conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )
      TRUE
    },
    error = function(e) {
      log_msg("✖ ERROR in ", path, " :: ", conditionMessage(e))
      FALSE
    }
  )
  
  t1 <- Sys.time()
  log_msg("⏱ Finished: ", path, " (", round(as.numeric(difftime(t1, t0, units = "secs")), 1), "s)")
  
  if (!ok) stop("Pipeline stopped because a step failed: ", path)
  gc()
  invisible(ok)
}

# ------------------------------------------------------------
# 0.1) Pre-chequeos (inputs y carpetas)
# ------------------------------------------------------------
log_msg("============================================================")
log_msg("START PIPELINE")
log_msg("Project root: ", here::here())
log_msg("R version: ", R.version.string)
log_msg("Timezone (CFG): ", CFG$project$timezone)
log_msg("============================================================")

# Insumos mínimos
if (!fs::dir_exists(CFG$dirs$data_eod))   log_msg("⚠ Falta carpeta EOD: ", CFG$dirs$data_eod)
if (!fs::dir_exists(CFG$dirs$data_ipc))   log_msg("⚠ Falta carpeta IPC: ", CFG$dirs$data_ipc)
if (!fs::dir_exists(CFG$dirs$data_imacec))log_msg("⚠ Falta carpeta IMACEC: ", CFG$dirs$data_imacec)

if (!file.exists(CFG$files$ipc_excel))    log_msg("⚠ Falta archivo IPC: ", CFG$files$ipc_excel)
if (!file.exists(CFG$files$imacec_excel)) log_msg("⚠ Falta archivo IMACEC: ", CFG$files$imacec_excel)

# ------------------------------------------------------------
# 1) Pipeline (en orden)
# ------------------------------------------------------------
steps <- list(
  c("02_eod_extract_individuals.R", NULL),
  c("03_eod_calendar_audit.R", NULL),
  c("04_nominal_quarterly_aggregation.R", NULL),
  c("05_ipc_build_index.R", NULL),
  c("06_real_income_and_quarterly.R", NULL),
  c("07_figures_wages_real.R", NULL),
  
  # desempleo
  c("08_eod_extract_unemployment_individuals.R", NULL),
  c("09_unemployment_quarterly_rate.R", NULL),
  c("10_figures_unemployment.R", NULL),
  
  # imacec / empleo / productividad
  c("11_imacec_quarterly_indices.R", NULL),
  c("12_employment_and_productivity.R", "12_employment_and_productivity_indices.R"),
  c("13_figures_imacec_employment_productivity.R", NULL),
  
  # CV adjusted
  c("14_cv_adjusted_mean_build.R", NULL),
  c("15_figures_cv_adjusted_mean.R", NULL)
)

t_start <- Sys.time()

for (s in steps) {
  primary  <- s[[1]]
  fallback <- if (length(s) >= 2) s[[2]] else NULL
  path <- resolve_step(primary, fallback)
  run_step(path)
}

t_end <- Sys.time()

# Guardar sessionInfo
si_file <- fs::path(CFG$dirs$logs, paste0("sessionInfo_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt"))
save_session_info(si_file)

log_msg("============================================================")
log_msg("PIPELINE COMPLETED SUCCESSFULLY")
log_msg("Total time (min): ", round(as.numeric(difftime(t_end, t_start, units = "mins")), 2))
log_msg("Log saved at: ", log_file)
log_msg("sessionInfo saved at: ", si_file)
log_msg("============================================================")
