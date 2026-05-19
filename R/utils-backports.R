# Copied from the R 4.6.0 implementation of array2DF() on 2026-05-19

if (getRversion() < "4.3.0") {
  array2DF <- function(
    x,
    responseName = "Value",
    sep = "",
    base = list(LETTERS),
    simplify = TRUE,
    allowLong = TRUE
  ) {
    .df_helper <- function(x) {
      if (!is.list(x)) {
        return(integer(0))
      }
      if (!all(vapply(x, inherits, TRUE, "data.frame"))) {
        return(integer(0))
      }
      if (length(unique(vapply(x, ncol, 1L))) > 1L) {
        return(integer(0))
      }
      if (length(unique(lapply(x, colnames))) > 1L) {
        return(integer(0))
      }
      return(vapply(x, nrow, 1L))
    }
    .unvec_helper <- function(x) {
      if (!is.list(x)) {
        return(integer(0))
      }
      if (!all(vapply(x, is.atomic, TRUE))) {
        return(integer(0))
      }
      if (!all(vapply(x, function(v) is.null(names(v)), TRUE))) {
        return(integer(0))
      }
      return(vapply(x, length, 1L))
    }
    keys <- do.call(
      expand.grid,
      c(
        dimnames(provideDimnames(x, sep = sep, base = base)),
        KEEP.OUT.ATTRS = FALSE,
        stringsAsFactors = FALSE
      )
    )
    vals <- NULL
    if (simplify) {
      dfrows <- .df_helper(x)
      if (length(dfrows)) {
        return(cbind(
          keys[rep(seq_along(dfrows), dfrows), , drop = FALSE],
          do.call(rbind, x)
        ))
      }
      if (allowLong) {
        unvecrows <- .unvec_helper(x)
        if (length(unvecrows)) {
          return(cbind(
            keys[rep(seq_along(unvecrows), unvecrows), , drop = FALSE],
            structure(data.frame(V = do.call(c, x)), names = responseName)
          ))
        }
      }
      x <- simplify2array(c(x))
      if (is.array(x)) {
        vals <- asplit(x, 1L)
        if (is.null(names(vals))) {
          names(vals) <- paste0(responseName, sep, seq_along(vals))
        }
      }
    }
    if (is.null(vals)) {
      vals <- list(c(x))
      names(vals) <- responseName
    }
    cbind(keys, list2DF(vals))
  }
}
