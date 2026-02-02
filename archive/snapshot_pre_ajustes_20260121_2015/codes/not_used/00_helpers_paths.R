# scripts/00_helpers_paths.R
pkgs <- c("here","fs","readr","ggplot2","stringi")
to_install <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")
invisible(lapply(pkgs, library, character.only = TRUE))
here::i_am("scripts/00_helpers_paths.R")  # si mantienes 'Scripts', cambia a 'Scripts/00_helpers_paths.R'

sanitize_name <- function(x){
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- gsub("[^A-Za-z0-9_\\-]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x <- tolower(x)
  x
}
save_plot <- function(p, name, dir = here("auditoria_variables","graficos"),
                      width=8, height=5, dpi=300, device=c("png","pdf")){
  device <- match.arg(device); fs::dir_create(dir)
  f <- fs::path(dir, paste0(sanitize_name(name), ".", device))
  if (inherits(p, "gg")) ggplot2::ggsave(f, p, width=width, height=height, dpi=dpi, units="in", device=device)
  else { if(device=="png") png(f, w=width, h=height, units="in", res=dpi) else pdf(f, w=width, h=height)
    print(p); dev.off() }
  invisible(f)
}
save_table <- function(x, name, dir = here("auditoria_variables","tablas")){
  fs::dir_create(dir); f <- fs::path(dir, paste0(sanitize_name(name), ".csv")); readr::write_csv(x, f); invisible(f)
}
message("Helpers cargados. here() => ", here())

here::i_am("scripts/00_helpers_paths.R")
