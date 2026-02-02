# scripts/make_all.R
source("scripts/_template_header.R")
source("scripts/00_build_utils.R")

# 1) Reconstruir audit_ingsueld_trimestral si falta
audit_out <- here("auditoria_variables","tablas","audit_ingsueld_trimestral.csv")
if (need_build(audit_out, deps = fs::dir_ls(here("data","eod"), regexp="\\.(sav|dta)$"))) {
  message("▶ Generando audit_ingsueld_trimestral…")
  source("scripts/01_eod_audit.R")  # tu script que lo genera y guarda en auditoria_variables/tablas
} else {
  message("✔ audit_ingsueld_trimestral OK")
}

# 2) Reconstruir IPC trimestral si falta
ipc_out <- here("auditoria_variables","tablas","ipc_trimestral_base2018.csv")
if (need_build(ipc_out, deps = here("data","ipc","PEM_VAR_IPC_2018_HIST.xlsx"))) {
  message("▶ Generando IPC trimestral…")
  source("scripts/02_ipc_trimestral.R")  # tu script IPC que exporta ipc_trimestral_base2018.csv
} else {
  message("✔ ipc_trimestral_base2018 OK")
}

# 3) Reconstruir salarios reales 2008–2022 si falta
sal0822_out <- here("auditoria_variables","tablas","salarios_reales_trimestrales_2008_2022.csv")
if (need_build(sal0822_out, deps = c(audit_out, ipc_out))) {
  message("▶ Generando salarios reales 2008–2022…")
  source("scripts/03_salarios_ipc_2008_2022.R")  # tu script que exporta tablas + gráficos
} else {
  message("✔ salarios_reales_trimestrales_2008_2022 OK")
}

# 4) Análisis metodológico (si quieres que también se regenere)
message("▶ Corriendo análisis metodológico…")
source("scripts/04_analisis_metodologico.R")

message("🎯 Pipeline OK.")
