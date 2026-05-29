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
