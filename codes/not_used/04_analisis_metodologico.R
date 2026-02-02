# ===============================================================
# Análisis metodológico (chequeos y validaciones básicas)
# - Usa tablas ya generadas
# - Exporta 2 gráficos de control
# ===============================================================

pkgs <- c("tidyverse","readr","scales","here")
invisible(lapply(pkgs, function(p) if (!requireNamespace(p, quietly=TRUE)) install.packages(p)))
lapply(pkgs, library, character.only = TRUE)

source(here("scripts","00_helpers_paths.R"))

# Entradas
f_sal  <- here("auditoria_variables","tablas","salarios_reales_trimestrales_2008_2022.csv")
f_anu  <- here("auditoria_variables","tablas","salarios_ipc_anual_2008_2022.csv")
f_ipc  <- here("auditoria_variables","tablas","ipc_trimestral_base2018.csv")
stopifnot(file.exists(f_sal), file.exists(f_anu), file.exists(f_ipc))

sal_q  <- readr::read_csv(f_sal, show_col_types = FALSE)
sal_an <- readr::read_csv(f_anu, show_col_types = FALSE)
ipc_q  <- readr::read_csv(f_ipc, show_col_types = FALSE)

# 1) Comparación anual nominal vs real
g1 <- sal_an |>
  pivot_longer(c(salario_nominal_prom, salario_real_prom),
               names_to="serie", values_to="valor") |>
  mutate(serie = recode(serie,
                        salario_nominal_prom = "Nominal",
                        salario_real_prom    = "Real")) |>
  ggplot(aes(year, valor, color=serie)) +
  geom_line(linewidth=1) +
  scale_y_continuous(labels = label_comma()) +
  labs(title="Promedios anuales de salario (2008–2022)",
       x=NULL, y="Pesos", color=NULL) +
  theme_minimal(base_size=12) +
  theme(legend.position="bottom")
save_plot(g1, "audit_ingsueld_media_log (promedios anuales)")

# 2) Correlación trimestral salario real vs IPC
g2 <- sal_q |>
  ggplot(aes(ipc_trimestre, salario_real)) +
  geom_point(alpha=.5) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(labels = label_comma()) +
  labs(title="Relación salario real vs IPC (trimestral)",
       x="IPC (2018=100)", y="Salario real (pesos)") +
  theme_minimal(base_size=12)
save_plot(g2, "audit_ingsueld_dispersion")

message("✔ Análisis metodológico exportado a auditoria_variables/graficos")
