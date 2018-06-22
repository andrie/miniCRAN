# Thin wrapper around utils::download.packages
download_packages <- function(pkgs, destdir, available, repos, contriburl, type, quiet){
  utils::download.packages(
    pkgs = pkgs,
    destdir = destdir,
    available = available,
    repos = repos,
    contriburl = contriburl,
    type = type, 
    quiet = quiet
  )
}

# Thin wrapper around write_PACKAGES
write_packages <- function(dir, type) {
  tools::write_PACKAGES(dir = dir, type = type)
}


