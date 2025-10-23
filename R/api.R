#' List available datasets
#' @return tibble with dataset id, outcome, measure, source, license
#' @export
list_datasets <- function() {
  reg <- system.file('registry', 'registry.csv', package = 'nmadatasets', mustWork = FALSE)
  if (!nzchar(reg)) reg <- file.path('inst','registry','registry.csv')
  if (!file.exists(reg)) tibble::tibble()
  else utils::read.csv(reg, stringsAsFactors = FALSE) |> tibble::as_tibble()
}

#' Get a dataset by name
#' @param name dataset id from list_datasets()$id
#' @return nma_data object
#' @export
get_dataset <- function(name) {
  stopifnot(is.character(name), length(name) == 1L)
  # Detect available files for this dataset id
  rbase <- system.file('extdata', package = 'nmadatasets')
  if (!nzchar(rbase)) rbase <- file.path('inst','extdata')

  base_csv    <- file.path(rbase, paste0(name, '.csv'))
  studies_csv <- file.path(rbase, paste0(name, '_studies.csv'))
  contrast_csv<- file.path(rbase, paste0(name, '_contrast.csv'))
  nodes_csv   <- file.path(rbase, paste0(name, '_nodes.csv'))
  meta_json   <- file.path(rbase, paste0(name, '_meta.json'))

  arm <- NULL; studies <- NULL; contrast <- NULL; nodes <- NULL

  if (file.exists(base_csv)) {
    arm <- utils::read.csv(base_csv, check.names = FALSE)
  } else if (file.exists(studies_csv)) {
    # Many datasets are shipped as *_studies.csv representing arm-level
    arm <- utils::read.csv(studies_csv, check.names = FALSE)
    studies <- arm
  }

  if (file.exists(contrast_csv)) contrast <- utils::read.csv(contrast_csv, check.names = FALSE)
  if (file.exists(nodes_csv))    nodes    <- utils::read.csv(nodes_csv, check.names = FALSE)

  meta <- if (file.exists(meta_json)) jsonlite::read_json(meta_json, simplifyVector = TRUE) else list(id = name)

  # If neither arm nor contrast available, error clearly
  if (is.null(arm) && is.null(contrast)) {
    stop("Dataset '", name, "' is not bundled (no arm or contrast files found).")
  }

  out <- structure(list(arm = arm, contrast = contrast, nodes = nodes, studies = studies, meta = meta), class = 'nma_data')
  validate_nma(out)
  out
}
