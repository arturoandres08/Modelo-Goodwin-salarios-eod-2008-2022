# ===============================================================
# Reconstrucción robusta: audit_ingsueld_trimestral.csv desde .sav
# Lee todos los .sav en data/eod/, detecta ingreso + ponderador,
# construye promedio ponderado trimestral y genera auditorías + gráficos
# ===============================================================

pkgs <- c("tidyverse","haven","fs","janitor","stringr","lubridate","scales","here")
invisible(lapply(pkgs, function(p) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)))
lapply(pkgs, library, character.only = TRUE)

# Helpers (rutas + save_plot/save_table)
source(here("scripts","00_helpers_paths.R"))

# ------------------- 0) Configuración -------------------
dir_eod <- here("data","eod")
stopifnot(dir_exists(dir_eod))

# meses posibles en nombres
mes_map <- c(
  "enero"="01","febrero"="02","marzo"="03","abril"="04","mayo"="05","junio"="06",
  "julio"="07","agosto"="08","septiembre"="09","setiembre"="09","octubre"="10",
  "noviembre"="11","diciembre"="12"
)

tri_map <- c("01"="T1","02"="T1","03"="T1",
             "04"="T2","05"="T2","06"="T2",
             "07"="T3","08"="T3","09"="T3",
             "10"="T4","11"="T4","12"="T4")

q_num <- function(tri) match(tri, c("T1","T2","T3","T4"))

# patrones candidatos (ajusta/añade si usas otros nombres)
cand_ingreso <- c("ing.*suel", "ing.*sal", "suel.*liqu", "suel.*mono", "salario", "ingreso.*labor")
cand_peso    <- c("^pond$", "ponder", "factor", "fact.*exp", "peso", "w[0-9]*")

pick_first <- function(nms, patterns) {
  for (pat in patterns) {
    idx <- str_which(nms, regex(pat, ignore_case = TRUE))
    if (length(idx) > 0) return(nms[idx[1]])
  }
  NA_character_
}

# lectura con encoding seguro
read_sav_safe <- function(path) {
  haven::read_sav(path, user_na = TRUE)
}

# extraer year/mes desde nombre archivo
parse_fecha_from_fname <- function(fname) {
  f <- tolower(fname)
  yr <- str_match(f, "(19|20)\\d{2}")[,1]
  mes_txt <- names(mes_map)[str_detect(f, names(mes_map))] |> dplyr::coalesce(NA_character_)
  mes <- ifelse(!is.na(mes_txt), mes_map[mes_txt][1], NA_character_)
  list(year = as.integer(yr), month = mes)
}

# weighted mean robusto
wmean <- function(x, w) {
  if (all(is.na(x)) || all(is.na(w))) return(NA_real_)
  sum(x * w, na.rm = TRUE) / sum(w[!is.na(x)], na.rm = TRUE)
}

# ------------------- 1) Enumerar archivos EOD -------------------
sav_files <- fs::dir_ls(dir_eod, regexp = "\\.sav$", recurse = FALSE, type = "file")

if (length(sav_files) == 0) stop("No hay archivos .sav en data/eod")

# ------------------- 2) Procesar archivo a archivo -------------------
diag_list  <- list()
flags_list <- list()
rows_list  <- list()

for (f in sav_files) {
  fname <- fs::path_file(f)
  info  <- parse_fecha_from_fname(fname)
  yr    <- info$year
  mm    <- info$month
  
  # si no detecta año o mes, intenta un segundo patrón (p. ej. 'junio2018.sav' o '2018junio.sav')
  if (is.na(yr) || is.na(mm)) {
    # busca cualquier 'mesYYYY' o 'YYYYmes'
    for (m in names(mes_map)) {
      if (str_detect(tolower(fname), paste0(m, "(19|20)\\d{2}"))) { mm <- mes_map[m]; break }
      if (str_detect(tolower(fname), paste0("(19|20)\\d{2}", m))) { mm <- mes_map[m]; break }
    }
    if (is.na(yr)) {
      yr2 <- str_match(tolower(fname), "(19|20)\\d{2}")[,1]
      yr  <- ifelse(length(yr2)>0, as.integer(yr2), NA_integer_)
    }
  }
  
  trimes <- ifelse(!is.na(mm), tri_map[mm], NA_character_)
  
  # lee
  df <- tryCatch(read_sav_safe(f), error = function(e) NULL)
  if (is.null(df)) {
    flags_list[[fname]] <- tibble(archivo=fname, year=yr, month=mm, trimestre=trimes,
                                  var_ingreso=NA, var_peso=NA, flag_ok=FALSE, motivo="fallo lectura")
    next
  }
  
  nms <- names(df)
  var_ing <- pick_first(nms, cand_ingreso)
  var_w   <- pick_first(nms, cand_peso)
  
  # diagnóstico de coincidencias
  diag_list[[fname]] <- tibble(
    archivo        = fname,
    year           = yr,
    month          = mm,
    trimestre      = trimes,
    matches_ingreso = paste(
      nms[str_which(nms, regex(paste(cand_ingreso, collapse = "|"), ignore_case = TRUE))],
      collapse = " | "
    ),
    matches_peso = paste(
      nms[str_which(nms, regex(paste(cand_peso, collapse = "|"), ignore_case = TRUE))],
      collapse = " | "
    )
  )
    
  
  # flags de selección
  ok <- !is.na(var_ing) && !is.na(var_w)
  
  flags_list[[fname]] <- tibble(
    archivo=fname, year=yr, month=mm, trimestre=trimes,
    var_ingreso = var_ing, var_peso = var_w,
    flag_ok = ok, motivo = ifelse(ok, NA, "no se encontró ingreso o ponderador")
  )
  
  if (!ok || is.na(yr) || is.na(trimes)) next
  
  # a numérico
  x <- suppressWarnings(as.numeric(df[[var_ing]]))
  w <- suppressWarnings(as.numeric(df[[var_w]]))
  
  # limpiezas suaves (descartar negativos imposibles)
  if (all(x < 0, na.rm = TRUE)) x <- abs(x)
  x[x < 0] <- NA
  w[w <= 0] <- NA
  
  wm <- wmean(x, w)
  
  rows_list[[fname]] <- tibble(
    year = as.integer(yr),
    trimestre = as.character(trimes),
    wmean_adj = as.numeric(wm),
    n_obs = sum(!is.na(x) & !is.na(w)),
    archivo = fname,
    var_ingreso = var_ing,
    var_peso = var_w
  )
}

audit_trimestral <- dplyr::bind_rows(rows_list) |>
  arrange(year, match(trimestre, c("T1","T2","T3","T4")))

audit_flags <- dplyr::bind_rows(flags_list) |>
  arrange(year, trimestre, archivo)

audit_diag  <- dplyr::bind_rows(diag_list) |>
  arrange(year, trimestre, archivo)

# ------------------- 3) Guardar tablas -------------------
save_table(audit_trimestral, "audit_ingsueld_trimestral")
save_table(audit_flags,      "audit_flags")
save_table(audit_diag,       "audit_ingsueld_diagnostico_flags")

# ------------------- 4) Gráficos de validación -------------------
# Serie de salario promedio
g_series <- audit_trimestral |>
  mutate(t_index = year + q_num(trimestre)/10) |>
  ggplot(aes(t_index, wmean_adj)) +
  geom_line(linewidth=.9) +
  labs(title="Salario (promedio ponderado) trimestral",
       x="Año (trimestre)", y="Pesos") +
  theme_minimal(base_size = 12)
save_plot(g_series, "audit_ingsueld_media")

# Dispersión de archivos: variabilidad por trimestre
g_disp <- audit_trimestral |>
  mutate(tri = paste0(year,"-",trimestre)) |>
  ggplot(aes(tri, wmean_adj)) +
  geom_point(alpha=.7) +
  theme_minimal(base_size=12) +
  theme(axis.text.x = element_text(angle=90, vjust=.5, size=8)) +
  labs(title="Chequeo por archivo/trimestre", x=NULL, y="Promedio ponderado")
save_plot(g_disp, "audit_ingsueld_dispersion_log")

message("✔ Listo: audit_ingsueld_trimestral + auditorías generadas.")
