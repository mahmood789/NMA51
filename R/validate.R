#' Validate an nma_data object
#' @param x nma_data
#' @export
validate_nma <- function(x) {
  stopifnot(inherits(x, 'nma_data'))
  ab <- x$arm
  req <- c('study','treatment')
  miss <- setdiff(req, names(ab))
  if (length(miss)) stop('arm missing: ', paste(miss, collapse = ', '))
  if (any(duplicated(ab[c('study','treatment')]))) warning('Duplicate study-treatment rows found.')
  if ('measure' %in% names(ab)) {
    if (length(unique(stats::na.omit(ab$measure))) > 1) 
      warning('Multiple measures present; adapters will use x$meta$preferred_measure where possible.')
  }
  invisible(x)
}
