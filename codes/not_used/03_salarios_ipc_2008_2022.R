# ===============================================================
# Salarios e IPC 2008–2022 (autocontenido, repo estandarizado)
# Lee insumos de data/eod & auditoria_variables, exporta a
# auditoria_variables/tablas y auditoria_variables/graficos
# ===============================================================

# ---- 0) Paquetes + helpers ----
pkgs <- c("tidyverse","readr","janitor","scales","fs","here")
to_install <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")
invisible(lapply(pkgs, library, character.only = TRUE))

# Cargar helpers (define save_plot, save_table y fija here() al root)
source("scripts/00_helpers_paths.R")

# ---- 1) Rutas lógicas del repo ----
dir_eod  <- here("data", "eod")
dir_ipc  <- here("data", "ipc")
dir_av   <- here("auditoria_variables")  # carpeta madre para salidas
fs::dir_create(dir_av)

# ---- 2) Utilidades locales ----
to_trimester <- function(m) dplyr::case_when(
  m %in% 1:3  ~ "T1",
  m %in% 4:6  ~ "T2",
  m %in% 7:9  ~ "T3",
  m %in% 10:12~ "T4",
  TRUE ~ NA_character_
)
q_num <- function(tri) match(tri, c("T1","T2","T3","T4"))

# ---- 3) Cargar / reconstruir salarios_reales_trimestrales ----
# Buscamos primero si ya existe la tabla consolidada en auditoria_variables
f_salarios <- here("auditoria_variables", "salarios_reales_trimestrales.csv")

if (file.exists(f_salarios)) {
  message("✔ Cargando 'salarios_reales_trimestrales.csv' desde auditoria_variables/")
  sal_full <- readr::read_csv(f_salarios, show_col_types = FALSE) %>% clean_names()
} else {
  message("ℹ No existe 'salarios_reales_trimestrales.csv'. Se intentará reconstruir…")
  
  # Insumos mínimos
  f_audit <- c(
    here("auditoria_variables", "audit_ingsueld_trimestral.csv"),
    here("data", "eod", "audit_ingsueld_trimestral.csv")
  )[file.exists(c(
    here("auditoria_variables", "audit_ingsueld_trimestral.csv"),
    here("data", "eod", "audit_ingsueld_trimestral.csv")
  ))][1]
  
  f_ipc_q <- here("auditoria_variables", "ipc_trimestral_base2018.csv")
  
  if (is.na(f_audit) || !file.exists(f_audit)) {
    stop("No encuentro 'audit_ingsueld_trimestral.csv' en auditoria_variables/ ni en data/eod/.")
  }
  if (!file.exists(f_ipc_q)) {
    stop("No encuentro 'ipc_trimestral_base2018.csv' en auditoria_variables/. Genera ese insumo primero.")
  }
  
  audit_quarter  <- readr::read_csv(f_audit, show_col_types = FALSE) %>% clean_names()
  ipc_trimestral <- readr::read_csv(f_ipc_q, show_col_types = FALSE) %>% clean_names()
  
  # Esperamos columnas: year, month, wmean_adj en audit_quarter;
  # y year, trimestre, ipc_trimestre en ipc_trimestral.
  if (!all(c("year","month","wmean_adj") %in% names(audit_quarter)))
    stop("El archivo de sueldos trimestral no tiene columnas esperadas: year, month, wmean_adj.")
  if (!all(c("year","trimestre","ipc_trimestre") %in% names(ipc_trimestral)))
    stop("El IPC trimestral no tiene columnas esperadas: year, trimestre, ipc_trimestre.")
  
  sal_full <- audit_quarter %>%
    mutate(trimestre = to_trimester(month)) %>%
    left_join(ipc_trimestral, by = c("year","trimestre")) %>%
    mutate(salario_real = wmean_adj / (ipc_trimestre / 100))
  
  readr::write_csv(sal_full, f_salarios)
  message("✔ Reconstruido y guardado: auditoria_variables/salarios_reales_trimestrales.csv")
}

# ---- 4) Filtrar 2008–2022 y asegurar variables clave ----
has_q <- "q" %in% names(sal_full)

sal_0822 <- sal_full %>%
  mutate(
    trimestre    = if (has_q) ifelse(is.na(trimestre), q, trimestre) else trimestre,
    t_index      = year + q_num(trimestre) / 10,
    salario_real = wmean_adj / (ipc_trimestre / 100)
  ) %>%
  filter(year >= 2008, year <= 2022) %>%
  arrange(year, t_index)

# Guardar dataset filtrado (tablas)
save_table(sal_0822, "salarios_reales_trimestrales_2008_2022")

# ---- 5) Promedios anuales (útil para series limpias) ----
anual_0822 <- sal_0822 %>%
  group_by(year) %>%
  summarise(
    salario_nominal_prom = mean(wmean_adj,    na.rm = TRUE),
    salario_real_prom    = mean(salario_real, na.rm = TRUE),
    ipc_prom             = mean(ipc_trimestre,na.rm = TRUE),
    .groups = "drop"
  )
save_table(anual_0822, "salarios_ipc_anual_2008_2022")

# ---- 6) Gráfico A — Nominal vs Real (pesos) ----
pA <- sal_0822 %>%
  select(t_index, wmean_adj, salario_real) %>%
  pivot_longer(c(wmean_adj, salario_real), names_to = "serie", values_to = "valor") %>%
  mutate(serie = recode(serie,
                        wmean_adj    = "Salario nominal",
                        salario_real = "Salario real")) %>%
  ggplot(aes(t_index, valor, color = serie)) +
  geom_line(linewidth = 0.9) +
  scale_y_continuous(labels = label_comma()) +
  scale_color_manual(values = c("Salario nominal" = "#2E86AB",
                                "Salario real"    = "#C0392B")) +
  labs(title = "Salario medio ponderado: nominal vs real (2008–2022)",
       subtitle = "Pesos corrientes vs reales (IPC base 2018=100)",
       x = "Año (trimestre)", y = "Pesos", color = NULL) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, size = 8),
        legend.position = "bottom")
save_plot(pA, "A_salario_nominal_vs_real_2008_2022", width = 10, height = 5, dpi = 120)

# ---- 7) Gráfico B — Índices (base 2018 = 100) ----
base_nom_2018  <- mean(sal_0822$wmean_adj[sal_0822$year == 2018],  na.rm = TRUE)
base_real_2018 <- mean(sal_0822$salario_real[sal_0822$year == 2018], na.rm = TRUE)

indices <- sal_0822 %>%
  mutate(
    idx_nom  = 100 * wmean_adj    / base_nom_2018,
    idx_real = 100 * salario_real / base_real_2018
  ) %>%
  select(t_index, idx_nom, idx_real, ipc_trimestre) %>%
  pivot_longer(c(idx_nom, idx_real, ipc_trimestre),
               names_to = "serie", values_to = "indice") %>%
  mutate(serie = recode(serie,
                        idx_nom       = "Salario nominal (índice)",
                        idx_real      = "Salario real (índice)",
                        ipc_trimestre = "IPC (índice)"))
pB <- ggplot(indices, aes(t_index, indice, color = serie)) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = c("IPC (índice)" = "#6C757D",
                                "Salario nominal (índice)" = "#2E86AB",
                                "Salario real (índice)" = "#C0392B")) +
  labs(title = "IPC y salarios (índice 2018 = 100, 2008–2022)",
       x = "Año (trimestre)", y = "Índice (2018=100)", color = NULL) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, size = 8),
        legend.position = "bottom")
save_plot(pB, "B_ipc_vs_salarios_indices_2008_2022", width = 10, height = 5, dpi = 120)

# ---- 8) Gráfico C — Doble eje (pesos e índice) ----
scale_factor <- base_nom_2018 / 100
sal_plot <- sal_0822 %>% mutate(ipc_scaled = ipc_trimestre * scale_factor)

pC <- ggplot(sal_plot, aes(t_index)) +
  geom_line(aes(y = wmean_adj,  color = "Salario nominal (pesos)"), linewidth = 0.9) +
  geom_line(aes(y = ipc_scaled, color = "IPC (escala secundaria)"),
            linewidth = 0.9, linetype = 2) +
  scale_y_continuous(
    name = "Pesos", labels = label_comma(),
    sec.axis = sec_axis(~ . / scale_factor, name = "Índice (2018=100)")
  ) +
  scale_color_manual(values = c("Salario nominal (pesos)" = "#2E86AB",
                                "IPC (escala secundaria)" = "#6C757D")) +
  labs(title = "Salario nominal y IPC (doble eje, 2008–2022)",
       subtitle = "Eje derecho: IPC base 2018=100; eje izquierdo: salarios en pesos",
       x = "Año (trimestre)", color = NULL) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, size = 8),
        legend.position = "bottom")
save_plot(pC, "C_salario_nominal_y_ipc_doble_eje_2008_2022", width = 10, height = 5, dpi = 120)

# ---- 9) Variaciones trimestrales y gráficos adicionales ----
sal_0822_var <- sal_0822 %>%
  arrange(year, t_index) %>%
  group_by(year) %>% ungroup() %>%    # seguro
  mutate(
    var_nominal = 100 * (wmean_adj    / lag(wmean_adj)    - 1),
    var_real    = 100 * (salario_real / lag(salario_real) - 1)
  )
save_table(sal_0822_var, "salarios_reales_trimestrales_con_variaciones")

p_trimestral <- sal_0822_var %>%
  select(t_index, wmean_adj, salario_real) %>%
  pivot_longer(c(wmean_adj, salario_real), names_to = "serie", values_to = "valor") %>%
  mutate(serie = recode(serie,
                        wmean_adj = "Salario nominal",
                        salario_real = "Salario real")) %>%
  ggplot(aes(t_index, valor, color = serie)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("Salario nominal" = "#2E86AB",
                                "Salario real"    = "#C0392B")) +
  scale_y_continuous(labels = comma) +
  labs(title = "Evolución trimestral: salario nominal vs real (2008–2022)",
       x = "Año (trimestre)", y = "Pesos", color = NULL) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, size = 8),
        legend.position = "bottom")
save_plot(p_trimestral, "D_salario_nominal_vs_real_trimestral", width = 10, height = 5, dpi = 120)

p_var <- sal_0822_var %>%
  select(t_index, var_nominal, var_real) %>%
  pivot_longer(-t_index, names_to = "serie", values_to = "variacion") %>%
  mutate(serie = recode(serie,
                        var_nominal = "Var. salario nominal",
                        var_real    = "Var. salario real")) %>%
  ggplot(aes(t_index, variacion, color = serie)) +
  geom_line(linewidth = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("Var. salario nominal" = "#2E86AB",
                                "Var. salario real"    = "#C0392B")) +
  labs(title = "Variación porcentual trimestral del salario nominal y real",
       x = "Año (trimestre)", y = "% trimestral", color = NULL) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, size = 8),
        legend.position = "bottom")
save_plot(p_var, "E_variacion_trimestral_nominal_vs_real", width = 10, height = 5, dpi = 120)

message("✔ Listo. Revisa 'auditoria_variables/tablas' y 'auditoria_variables/graficos'.")
