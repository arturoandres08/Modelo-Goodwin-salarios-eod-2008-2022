# scripts/_template_header.R
pkgs <- c("tidyverse","janitor","readr","fs","here","scales")
to_install <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")
invisible(lapply(pkgs, library, character.only = TRUE))

# Fija el root del proyecto y carga helpers (save_plot/save_table/sanitize_name)
source("scripts/00_helpers_paths.R")

# Carpetas canónicas del repo
dir_eod   <- here("data","eod")
dir_ipc   <- here("data","ipc")
dir_av    <- here("auditoria_variables")
dir_tab   <- here("auditoria_variables","tablas")
dir_gfx   <- here("auditoria_variables","graficos")
fs::dir_create(c(dir_av, dir_tab, dir_gfx))

# Conveniencias para series trimestrales
to_trimester <- function(m) dplyr::case_when(
  m %in% 1:3  ~ "T1", m %in% 4:6  ~ "T2",
  m %in% 7:9  ~ "T3", m %in% 10:12~ "T4", TRUE ~ NA_character_
)
q_num <- function(tri) match(tri, c("T1","T2","T3","T4"))
