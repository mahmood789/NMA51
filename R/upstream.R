#' Return the path to the package registry (installed or dev).
registry_path <- function() {
  p <- system.file('registry','registry.csv', package = 'nmadatasets', mustWork = FALSE)
  if (!nzchar(p)) p <- file.path('inst','registry','registry.csv')
  p
}

#' List upstream datasets only (registered but not embedded).
#' @return tibble with at least id, package, object columns
#' @export
list_upstream <- function() {
  p <- registry_path()
  if (!file.exists(p)) return(tibble::tibble())
  dat <- utils::read.csv(p, stringsAsFactors = FALSE)
  if (!'package' %in% names(dat)) dat$package <- NA_character_
  if (!'object' %in% names(dat)) dat$object <- NA_character_
  tibble::as_tibble(dat[!is.na(dat$package) & !is.na(dat$object), ])
}

#' Fetch a raw dataset from its upstream package
#' 
#' Use ids like 'gemtc__smoking' that were auto-registered.
#' @param id character
#' @return the object as provided by the upstream package (no normalization).
#' @export
sanitize_object_name <- function(obj) {
  # remove trailing annotations e.g. "parkinson_diff (parkinson-diff)"
  sub("\\s*\\(.*$", "", obj)
}

find_best_object_name <- function(pkg, obj) {
  items <- tryCatch(utils::data(package = pkg)$results, error = function(e) NULL)
  if (is.null(items) || !NROW(items)) return(sanitize_object_name(obj))
  items <- unique(as.character(as.data.frame(items, stringsAsFactors = FALSE)[['Item']]))
  target <- tolower(gsub("[^a-z0-9]", "", obj))
  cand <- items[ tolower(gsub("[^a-z0-9]", "", items)) == target ]
  if (length(cand) >= 1) return(cand[1])
  # fallback: prefix or contains match
  cand2 <- items[ grepl(paste0('^', gsub("[^a-z0-9]","", target)), gsub("[^a-z0-9]","", tolower(items))) ]
  if (length(cand2) >= 1) return(cand2[1])
  sanitize_object_name(obj)
}

get_upstream_raw <- function(id) {
  stopifnot(is.character(id), length(id) == 1L)
  p <- registry_path(); if (!file.exists(p)) stop('No registry found.')
  reg <- utils::read.csv(p, stringsAsFactors = FALSE)
  row <- reg[reg$id == id, , drop = FALSE]
  if (!nrow(row)) stop('No such id in registry: ', id)
  pkg <- row$package[1]; obj <- row$object[1]
  if (!requireNamespace(pkg, quietly = TRUE)) stop('Package not installed: ', pkg)
  obj2 <- find_best_object_name(pkg, obj)
  e <- new.env(parent = emptyenv())
  suppressWarnings(utils::data(list = obj2, package = pkg, envir = e))
  if (!exists(obj2, envir = e, inherits = FALSE)) {
    # try sanitized original as last resort
    obj3 <- sanitize_object_name(obj)
    suppressWarnings(utils::data(list = obj3, package = pkg, envir = e))
    if (!exists(obj3, envir = e, inherits = FALSE))
      stop('Upstream object not found: ', pkg, '::', obj)
    return(get(obj3, envir = e, inherits = FALSE))
  }
  get(obj2, envir = e, inherits = FALSE)
}
