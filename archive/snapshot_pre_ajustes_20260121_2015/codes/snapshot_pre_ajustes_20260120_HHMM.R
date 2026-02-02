# SNAPSHOT del proyecto (antes de ajustes)
# Crea: archive/snapshot_pre_ajustes_20260120_HHMM/
# Copia: codes, data/(interim/processed/validation), auditoria_variables, outputs, logs, y archivos clave.

library(fs)
library(here)

root <- here::here()
stamp <- format(Sys.time(), "%Y%m%d_%H%M")
snap_dir <- fs::path(root, "archive", paste0("snapshot_pre_ajustes_", stamp))
fs::dir_create(snap_dir)

# Qué cosas vamos a congelar (resultados + scripts)
items <- c(
  "codes",
  fs::path("data", "interim"),
  fs::path("data", "processed"),
  fs::path("data", "validation"),
  "auditoria_variables",
  "outputs",
  "logs",
  "README",
  # si tu .Rproj tiene otro nombre, no pasa nada: el script lo ignora si no existe
  "Trabajo_R_Tesis.Rproj",
  ".gitignore"
)

copy_one <- function(rel_path) {
  src <- fs::path(root, rel_path)
  if (!fs::file_exists(src) && !fs::dir_exists(src)) {
    message("↪ (no existe, se omite): ", rel_path)
    return(invisible(NULL))
  }
  dest <- fs::path(snap_dir, fs::path_file(src))
  if (fs::dir_exists(src)) {
    fs::dir_copy(src, dest, overwrite = TRUE)
    message("✔ Dir copiada: ", rel_path)
  } else {
    fs::file_copy(src, dest, overwrite = TRUE)
    message("✔ Archivo copiado: ", rel_path)
  }
  invisible(dest)
}

invisible(lapply(items, copy_one))

message("\n✅ SNAPSHOT LISTO en: ", snap_dir)
message("Abre esa carpeta y revisa que estén codes/, data/, auditoria_variables/ y outputs/.")
