# ===============================================================
# 00_build_utils.R
# Funciones para guardar tablas y figuras de forma segura
# ===============================================================

library(fs)
library(here)
library(readr)
library(ggplot2)

if (!exists("save_table")) {
  save_table <- function(df, name, dir = here("auditoria_variables", "tablas")) {
    dir_create(dir)
    path <- fs::path(dir, paste0(name, ".csv"))
    readr::write_csv(df, path)
    message("✔ Tabla guardada: ", path)
    invisible(path)
  }
}

if (!exists("save_plot")) {
  save_plot <- function(p, name, dir = here("auditoria_variables", "graficos"),
                        width = 10, height = 6, dpi = 300, device = "png") {
    dir_create(dir)
    path <- fs::path(dir, paste0(name, ".", device))
    ggsave(filename = path, plot = p, width = width, height = height, dpi = dpi)
    message("✔ Gráfico guardado: ", path)
    invisible(path)
  }
}
