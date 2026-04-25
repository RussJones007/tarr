# zzz.R
# Contains misc functions for the package

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.tarr <- list(
    tarr.conflicts.policy = list(
      can.mask = c("base")
    )
  )
  options(op.tarr)
  invisible()
}

.onAttach <- function(libname, pkgname) {
  
  core_pkgs <- c("rage", "tarr.pop")
  
  suppressPackageStartupMessages({
    for (pkg in core_pkgs) {
      if (!requireNamespace(pkg, quietly = TRUE)) {
        stop(
          "Package '", pkg, "' must be installed.",
          call. = FALSE
        )
      }
      library(pkg, character.only = TRUE)
    }
  })
      
  packageStartupMessage(
    "── Attaching tarr ecosystem ──\n",
    paste0("✔ ", core_pkgs, collapse = "\n")
  )
}
