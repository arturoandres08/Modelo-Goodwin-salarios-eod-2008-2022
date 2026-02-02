# scripts/00_build_utils.R
if (!requireNamespace("fs", quietly = TRUE)) install.packages("fs")
library(fs)

# ¿Hay que reconstruir un producto dado sus dependencias?
need_build <- function(out, deps) {
  if (!file_exists(out)) return(TRUE)
  out_time <- file_info(out)$modification_time
  any(vapply(deps, function(d) !file_exists(d) || file_info(d)$modification_time > out_time, logical(1)))
}

# Envoltorio seguro para guardar CSV en carpetas canónicas
save_csv_av <- function(df, name) {
  out <- here("auditoria_variables","tablas", paste0(sanitize_name(name), ".csv"))
  fs::dir_create(path_dir(out))
  readr::write_csv(df, out)
  message("✔ Guardado: ", out)
  invisible(out)
}

# Envoltorio para guardar gráficos en graficos/
save_plot_av <- function(p, name, width=10, height=5, dpi=120) {
  save_plot(p, name, dir = here("auditoria_variables","graficos"), width=width, height=height, dpi=dpi, device="png")
}
