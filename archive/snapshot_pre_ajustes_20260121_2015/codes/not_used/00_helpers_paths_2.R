# ===============================================================
# 00_helpers_paths.R
# Rutas del proyecto (data/raw, data/processed, auditoria_variables)
# ===============================================================

library(fs)
library(here)

# ---- DATA ----
DIR_RAW_EOD    <- here("data", "raw", "eod")
DIR_RAW_IPC    <- here("data", "raw", "ipc")
DIR_RAW_IMACEC <- here("data", "raw", "imacec")

DIR_PROCESSED  <- here("data", "processed")
DIR_INTERIM    <- here("data", "interim")
DIR_VALIDATION <- here("data", "validation")

# ---- OUTPUT DE AUDITORÍA ----
DIR_AUD_TAB <- here("auditoria_variables", "tablas")
DIR_AUD_FIG <- here("auditoria_variables", "graficos")

# Crear carpetas si no existen
dir_create(DIR_PROCESSED)
dir_create(DIR_INTERIM)
dir_create(DIR_VALIDATION)
dir_create(DIR_AUD_TAB)
dir_create(DIR_AUD_FIG)

# Checks mínimos
stopifnot(dir_exists(DIR_RAW_EOD))
stopifnot(dir_exists(DIR_RAW_IPC))
stopifnot(dir_exists(DIR_RAW_IMACEC))
