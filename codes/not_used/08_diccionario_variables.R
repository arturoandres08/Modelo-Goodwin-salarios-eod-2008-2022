# ==== Diccionario desde variable_comparison_full.xlsx =========================
library(readxl); library(janitor); library(stringr); library(dplyr); library(tidyr); library(purrr)

# Ruta del Excel
f_dict_xlsx <- here::here("auditoria_variables", "tablas", "variable_comparison_full.xlsx")

# Mapa de meses en español (formas largas y abreviadas)
mes_map_val <- c(
  "enero"="01","febrero"="02","marzo"="03","abril"="04","mayo"="05","junio"="06",
  "julio"="07","agosto"="08","septiembre"="09","setiembre"="09","octubre"="10",
  "noviembre"="11","diciembre"="12",
  "ene"="01","feb"="02","mar"="03","abr"="04","may"="05","jun"="06",
  "jul"="07","ago"="08","sep"="09","oct"="10","nov"="11","dic"="12"
)

# Patrones de columnas que podrían contener el NOMBRE de la variable de interés
pat_mes_var  <- "(^|_)(mes|mes_enc|meslev|mes_lev|mes_ent|mes_entrev|periodo_mes|fecha_mes|mes_muestra|mes_mues|meses)(_|$)"
pat_tri_var  <- "(^|_)(tri|trimestre|tri_enc|trim_enc|trim)(_|$)"
pat_ano_var  <- "(^|_)(ano|anio|year|ano_enc|periodo_anos|periodo_ano)(_|$)"

# Patrones para valores crudos (si en el Excel aparecen valores, no sólo nombres)
pat_val_mes  <- "(enero|febrero|marzo|abril|mayo|junio|julio|agosto|septiembre|setiembre|octubre|noviembre|diciembre|ene|feb|mar|abr|may|jun|jul|ago|sep|oct|nov|dic|^0?[1-9]$|^1[0-2]$)"
pat_val_tri  <- "^(t[1-4]|T[1-4]|1|2|3|4)$"
pat_val_ano  <- "^(19|20)\\d{2}$"

# Lee primera hoja (o todas si hay varias y las une verticalmente)
if (file.exists(f_dict_xlsx)) {
  sheets <- readxl::excel_sheets(f_dict_xlsx)
  dict_raw <- purrr::map_dfr(sheets, ~ readxl::read_xlsx(f_dict_xlsx, sheet = .x, guess_max = 1e6) %>%
                               mutate(.sheet = .x))
  dict_raw <- janitor::clean_names(dict_raw)
} else {
  dict_raw <- tibble()  # si no existe, seguimos con fallback por nombre de archivo
}

# Función auxiliar: primera columna cuyo nombre hace match con un patrón
first_col_match <- function(nms, pattern) {
  hit <- nms[str_detect(nms, regex(pattern, ignore_case = TRUE))]
  if (length(hit)) hit[1] else NA_character_
}

# Intento de extraer, por fila, el **nombre** de la columna que representa mes/tri/año
if (nrow(dict_raw)) {
  nms <- names(dict_raw)
  
  col_mes_var <- first_col_match(nms, pat_mes_var)
  col_tri_var <- first_col_match(nms, pat_tri_var)
  col_ano_var <- first_col_match(nms, pat_ano_var)
  
  # Si el Excel trae varias columnas candidatas, puedes añadir más `first_col_match(...)`
  # y luego coalesce() entre ellas. Aquí vamos con una por brevedad.
  
  # Columnas informativas que suelan existir: archivo y/o año
  col_archivo <- first_col_match(nms, "(archivo|file|nombre|nombre_archivo)")
  col_yearref <- first_col_match(nms, "(year_ref|anio_ref|ano_ref|anio|year)")
  
  dict_vars <- dict_raw %>%
    mutate(
      # normalizamos *posibles valores* de mes/trimestre/año si el Excel trae valores
      mes_val  = if (!is.na(col_mes_var)) .data[[col_mes_var]] else NA,
      tri_val  = if (!is.na(col_tri_var)) .data[[col_tri_var]] else NA,
      ano_val  = if (!is.na(col_ano_var)) .data[[col_ano_var]] else NA,
      # clave de archivo y/o año para vincular con .sav
      archivo  = if (!is.na(col_archivo)) as.character(.data[[col_archivo]]) else NA_character_,
      year_key = suppressWarnings(as.integer(if (!is.na(col_yearref)) .data[[col_yearref]] else NA))
    ) %>%
    mutate(
      # Limpieza de valores: a texto minúscula para mapear meses
      mes_val = tolower(as.character(mes_val)),
      tri_val = tolower(as.character(tri_val)),
      ano_val = ifelse(str_detect(as.character(ano_val), pat_val_ano), as.integer(ano_val), NA_integer_),
      # mes_num: si mes_val es texto, lo mapeamos; si es "1..12" también lo dejamos
      mes_num = dplyr::case_when(
        str_detect(mes_val, "^[0-9]+$") ~ stringr::str_pad(as.integer(mes_val), 2, pad = "0"),
        str_detect(mes_val %||% "", pat_val_mes) ~ unname(mes_map_val[mes_val]),
        TRUE ~ NA_character_
      ),
      tri_norm = dplyr::case_when(
        str_detect(tri_val %||% "", "^t[1-4]$") ~ toupper(tri_val),
        str_detect(tri_val %||% "", "^[1-4]$")  ~ paste0("T", tri_val),
        TRUE ~ NA_character_
      )
    ) %>%
    transmute(
      archivo = archivo,
      year_key = year_key,
      mes_var_nombre = col_mes_var,
      tri_var_nombre = col_tri_var,
      ano_var_nombre = col_ano_var,
      # valores (por si el Excel trae el valor ya calculado para ese archivo)
      mes_val = mes_num,                # "01".."12" si se pudo inferir
      tri_val = tri_norm,               # "T1".."T4" si se pudo inferir
      year_val = ano_val
    ) %>%
    # nos quedamos sólo con filas donde al menos hay una pista útil
    filter(!(is.na(mes_val) & is.na(tri_val) & is.na(year_val) &
               is.na(mes_var_nombre) & is.na(tri_var_nombre) & is.na(ano_var_nombre))) %>%
    distinct()
  
  message("✔ Diccionario desde variable_comparison_full.xlsx: ", nrow(dict_vars), " filas útiles.")
} else {
  dict_vars <- tibble()
  message("ℹ No se encontró variable_comparison_full.xlsx; se usará sólo fallback por nombre de archivo.")
}

# ==== Utilidades de normalización =================================================
to_trimester <- function(m) paste0("T", ceiling(as.integer(m)/3))
tri_map <- c("01"="T1","02"="T1","03"="T1",
             "04"="T2","05"="T2","06"="T2",
             "07"="T3","08"="T3","09"="T3",
             "10"="T4","11"="T4","12"="T4")

infer_year_from_fname <- function(path) {
  y <- stringr::str_extract(basename(path), "(19|20)\\d{2}")
  suppressWarnings(as.integer(y))
}
infer_month_from_fname <- function(path) {
  f <- tolower(basename(path))
  hit <- names(mes_map_val)[str_detect(f, paste0("\\b(", paste(names(mes_map_val), collapse="|"), ")\\b"))]
  if (length(hit)) return(unname(mes_map_val[hit[1]]))
  mm <- str_match(f, "(^|[^0-9])((0[1-9])|(1[0-2]))([^0-9]|$)")[,3]
  if (!is.na(mm)) return(mm)
  # códigos T1..T4 (primer mes del trimestre)
  if (str_detect(f, "t1")) return("01")
  if (str_detect(f, "t2")) return("04")
  if (str_detect(f, "t3")) return("07")
  if (str_detect(f, "t4")) return("10")
  NA_character_
}

# Dada una tabla ya leída (data.frame) y un registro de dict_vars para ese archivo,
# intenta extraer valores de mes/trimestre/año.
extract_date_from_df <- function(df, dict_row) {
  out <- list(year=NA_integer_, month_chr=NA_character_, trimestre=NA_character_, conf = 0)
  
  # 1) año: si trae nombre de columna de año, úsalo
  if (!is.na(dict_row$ano_var_nombre) && dict_row$ano_var_nombre %in% names(df)) {
    y <- suppressWarnings(as.integer(df[[dict_row$ano_var_nombre]][1]))
    if (is.finite(y)) { out$year <- y; out$conf <- out$conf + .4 }
  } else if (!is.na(dict_row$year_val)) {
    out$year <- dict_row$year_val; out$conf <- out$conf + .2
  }
  
  # 2) mes: por nombre de variable
  if (!is.na(dict_row$mes_var_nombre) && dict_row$mes_var_nombre %in% names(df)) {
    mv <- df[[dict_row$mes_var_nombre]][1]
    mv <- tolower(as.character(mv))
    if (str_detect(mv, "^[0-9]+$")) {
      out$month_chr <- str_pad(as.integer(mv), 2, pad="0")
      out$conf <- out$conf + .4
    } else if (str_detect(mv, paste0("^(", paste(names(mes_map_val), collapse="|"), ")$"))) {
      out$month_chr <- unname(mes_map_val[mv])
      out$conf <- out$conf + .4
    }
  } else if (!is.na(dict_row$mes_val)) {
    out$month_chr <- dict_row$mes_val; out$conf <- out$conf + .2
  }
  
  # 3) trimestre: por nombre de variable o por mes ya inferido
  if (!is.na(dict_row$tri_var_nombre) && dict_row$tri_var_nombre %in% names(df)) {
    tv <- toupper(as.character(df[[dict_row$tri_var_nombre]][1]))
    if (str_detect(tv, "^T[1-4]$")) { out$trimestre <- tv; out$conf <- out$conf + .4 }
    if (str_detect(tv, "^[1-4]$"))  { out$trimestre <- paste0("T", tv); out$conf <- out$conf + .4 }
  } else if (!is.na(dict_row$tri_val)) {
    out$trimestre <- dict_row$tri_val; out$conf <- out$conf + .2
  }
  
  # Coherencia mes ↔ trimestre
  if (!is.na(out$month_chr) && is.na(out$trimestre)) {
    out$trimestre <- unname(tri_map[out$month_chr]); out$conf <- out$conf + .1
  }
  if (!is.na(out$trimestre) && is.na(out$month_chr)) {
    out$month_chr <- switch(out$trimestre, T1="01", T2="04", T3="07", T4="10", NA)
  }
  out
}

# Inicializar lista para auditoría
auditorias_guardadas_lista <- list()

for (i in seq_len(nrow(files_tbl))) {
  
  f  <- files_tbl$path[i]
  bn <- fs::path_file(f)
  
  year_fallback <- infer_year_from_fname(bn)
  mon_fallback  <- infer_month_from_fname(bn)
  
  dict_row <- dict_vars %>%
    filter(!is.na(archivo)) %>%
    filter(stringr::str_detect(bn, fixed(archivo, ignore_case = TRUE))) %>%
    slice_head(n = 1)
  
  if (nrow(dict_row) == 0L) {
    dict_row <- dict_vars %>%
      filter(!is.na(year_key), year_key == year_fallback) %>%
      slice_head(n = 1)
  }
  
  df <- tryCatch(haven::read_sav(f, user_na = TRUE), error = function(e) NULL)
  if (is.null(df)) next
  df <- janitor::clean_names(df)
  
  date_info <- if (nrow(dict_row)) extract_date_from_df(df, dict_row)
  else list(year=NA_integer_, month_chr=NA_character_, trimestre=NA_character_, conf=0)
  
  year_final  <- dplyr::coalesce(date_info$year, year_fallback)
  month_chr   <- dplyr::coalesce(date_info$month_chr, mon_fallback)
  tri_final   <- dplyr::coalesce(date_info$trimestre, if (!is.na(month_chr)) unname(tri_map[month_chr]) else NA_character_)
  
  audit_mes_tri <- tibble::tibble(
    archivo = bn,
    year_detectado = year_final,
    month_detectado = month_chr,
    tri_detectado = tri_final,
    year_conf  = as.numeric(!is.na(date_info$year)) + as.numeric(!is.na(year_fallback))*0.5,
    mes_conf   = as.numeric(!is.na(date_info$month_chr)) + as.numeric(!is.na(mon_fallback))*0.5,
    tri_conf   = as.numeric(!is.na(date_info$trimestre)) + as.numeric(!is.na(tri_final))*0.25
  )
  
  # Guardar cada auditoría en la lista
  auditorias_guardadas_lista[[i]] <- audit_mes_tri
}

# Al finalizar el bucle
auditoria_det <- dplyr::bind_rows(auditorias_guardadas_lista)
readr::write_csv(auditoria_det,
                 here::here("auditoria_variables","tablas","auditoria_mes_trimestre_por_archivo_diccionario.csv")
)
message("✔ Auditoría mes/trimestre/año exportada correctamente (diccionario + fallback).")
