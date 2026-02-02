# auditoria_rapida.R
library(fs)

check_dirs <- c("data", "scripts", "auditoria_variables/graficos", "auditoria_variables/tablas", "auditoria_variables/respaldos")
missing <- check_dirs[!dir_exists(check_dirs)]
if (length(missing) > 0) warning("Carpetas faltantes: ", paste(missing, collapse = ", "))

csv_files <- dir_ls("auditoria_variables/tablas", regexp = "\\.csv$")
png_files <- dir_ls("auditoria_variables/graficos", regexp = "\\.(png|pdf)$")
message("Tablas exportadas: ", length(csv_files))
message("Gráficos exportados: ", length(png_files))
