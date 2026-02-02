# ===============================================================
# IPC trimestral (base 2018 = 100) desde histórico oficial
# Lee:   data/ipc/PEM_VAR_IPC_2018_HIST.xlsx
# Sale:  auditoria_variables/tablas/ipc_trimestral_base2018.csv
#        auditoria_variables/graficos/IPC_trimestral_2018_100.png
# ===============================================================

pkgs <- c("tidyverse","readxl","janitor","lubridate","stringr","scales","here")
invisible(lapply(pkgs, function(p) if (!requireNamespace(p, quietly=TRUE)) install.packages(p)))
lapply(pkgs, library, character.only = TRUE)

source(here("scripts","00_helpers_paths.R"))

f_ipc <- here("data","ipc","PEM_VAR_IPC_2018_HIST.xlsx")
stopifnot(file.exists(f_ipc))

ipc_raw <- readxl::read_excel(f_ipc, guess_max = 1e5) %>%
  janitor::clean_names()

nms <- names(ipc_raw)

# patrones más amplios
year_pat  <- "(^|_)(year|ano|anio)$"
month_pat <- "(^|_)(month|mes)$"
date_pat  <- "(^|_)(date|fecha|periodo)$"     # <— incluye 'periodo'
ipc_pat   <- "ipc|indice"                     # <— cualquier columna que contenga 'ipc' o 'indice'

# detectar columnas (no limitamos por tipo para IPC)
col_date  <- nms[str_detect(nms, regex(date_pat,  ignore_case = TRUE))][1]
col_year  <- nms[str_detect(nms, regex(year_pat,  ignore_case = TRUE))][1]
col_month <- nms[str_detect(nms, regex(month_pat, ignore_case = TRUE))][1]
col_ipc   <- nms[str_detect(nms, regex(ipc_pat,   ignore_case = TRUE))][1]

if (is.na(col_ipc)) {
  stop("No pude detectar la columna del índice IPC. Encabezados: ", paste(nms, collapse = ", "))
}

# normalización mensual robusta
ipc_m <- ipc_raw %>%
  {
    if (!is.na(col_date)) {
      mutate(.,
             date  = as.Date(.data[[col_date]]),
             # si 'periodo' no es Date, intentamos 'ym'
             date  = ifelse(is.na(date), as.Date(lubridate::ym(.data[[col_date]])), date) %>% as.Date(origin = "1970-01-01"),
             year  = lubridate::year(date),
             month = lubridate::month(date),
             ipc   = .data[[col_ipc]]
      )
    } else {
      if (is.na(col_year) || is.na(col_month)) {
        stop("No encuentro columnas de fecha (year/month o date/fecha/periodo). Encabezados: ",
             paste(nms, collapse = ", "))
      }
      mutate(.,
             year  = as.integer(.data[[col_year]]),
             month = as.integer(.data[[col_month]]),
             date  = lubridate::make_date(year, month, 1),
             ipc   = .data[[col_ipc]]
      )
    }
  } %>%
  # coerción numérica tolerante a comas decimales
  mutate(
    ipc = suppressWarnings(as.numeric(ipc)),
    ipc = ifelse(is.na(ipc) & is.character(.data[[col_ipc]]),
                 suppressWarnings(as.numeric(str_replace(.data[[col_ipc]], ",", "."))),
                 ipc)
  ) %>%
  filter(!is.na(year), !is.na(month), !is.na(ipc)) %>%
  arrange(year, month)

ipc_q <- ipc_m %>%
  mutate(trimestre = paste0("T", lubridate::quarter(date))) %>%
  group_by(year, trimestre) %>%
  summarise(ipc_trimestre = mean(ipc, na.rm = TRUE), .groups = "drop") %>%
  arrange(year, trimestre)

save_table(ipc_q, "ipc_trimestral_base2018")

g_ipc <- ipc_q %>%
  mutate(t_index = year + match(trimestre, c("T1","T2","T3","T4"))/10) %>%
  ggplot(aes(t_index, ipc_trimestre)) +
  geom_line(linewidth = 0.9) +
  labs(title = "IPC trimestral (2018 = 100)",
       x = "Año (trimestre)", y = "Índice",
       caption = "Fuente: PEM_VAR_IPC_2018_HIST.xlsx") +
  theme_minimal(base_size = 12)

save_plot(g_ipc, "IPC_trimestral_2018_100", width = 9, height = 4.8)

message("✔ IPC trimestral generado y guardado en auditoria_variables/{tablas,graficos}")
