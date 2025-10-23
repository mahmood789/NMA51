# Validate all embedded datasets in inst/extdata and summarize

suppressPackageStartupMessages({
  library(jsonlite)
})

here_ext <- function() {
  # Prefer local development extdata if present; otherwise installed path
  p_local <- file.path('inst','extdata')
  if (dir.exists(p_local)) return(p_local)
  p <- system.file('extdata', package = 'nmadatasets', mustWork = FALSE)
  if (!nzchar(p)) p <- p_local
  p
}

ext <- here_ext()
files <- list.files(ext, pattern = '\\.(csv)$', full.names = TRUE)
bases <- tools::file_path_sans_ext(basename(files))
ids <- unique(gsub('_(nodes|studies|contrast)$','', bases))

core_cols <- c('study','treatment','r','n','y','sd','measure','outcome')

summ <- data.frame(
  id = character(),
  has_arm = logical(),
  has_contrast = logical(),
  n_rows_arm = integer(),
  n_treatments = integer(),
  n_studies = integer(),
  has_nodes = logical(),
  covariates = character(),
  issues = character(),
  stringsAsFactors = FALSE
)

for (id in ids) {
  issues <- character()
  # discover files
  f_arm1 <- file.path(ext, paste0(id, '.csv'))
  f_arm2 <- file.path(ext, paste0(id, '_studies.csv'))
  f_con  <- file.path(ext, paste0(id, '_contrast.csv'))
  f_nodes<- file.path(ext, paste0(id, '_nodes.csv'))
  has_arm <- file.exists(f_arm1) || file.exists(f_arm2)
  has_con <- file.exists(f_con)
  has_nodes <- file.exists(f_nodes)

  arm <- NULL
  if (file.exists(f_arm1)) arm <- tryCatch(utils::read.csv(f_arm1, check.names = FALSE), error=function(e) NULL)
  if (is.null(arm) && file.exists(f_arm2)) arm <- tryCatch(utils::read.csv(f_arm2, check.names = FALSE), error=function(e) NULL)
  contrast <- if (has_con) tryCatch(utils::read.csv(f_con, check.names = FALSE), error=function(e) NULL) else NULL
  nodes <- if (has_nodes) tryCatch(utils::read.csv(f_nodes, check.names = FALSE), error=function(e) NULL) else NULL

  # quick checks
  if (has_arm && is.null(arm)) issues <- c(issues, 'arm file unreadable')
  if (has_con && is.null(contrast)) issues <- c(issues, 'contrast file unreadable')

  # stats + covariates detection
  n_rows <- if (!is.null(arm)) nrow(arm) else NA_integer_
  n_treat <- if (!is.null(arm) && 'treatment' %in% names(arm)) length(unique(na.omit(arm$treatment))) else NA_integer_
  n_stud  <- if (!is.null(arm) && 'study' %in% names(arm)) length(unique(na.omit(arm$study))) else NA_integer_

  extra_cols <- character()
  if (!is.null(arm)) {
    extra_cols <- setdiff(names(arm), core_cols)
    # flag empty study ids
    if ('study' %in% names(arm) && any(is.na(arm$study))) issues <- c(issues, 'missing study identifiers present')
    if ('treatment' %in% names(arm) && any(is.na(arm$treatment))) issues <- c(issues, 'missing treatment labels present')
    # check duplicates
    if (all(c('study','treatment') %in% names(arm))) {
      dups <- any(duplicated(arm[c('study','treatment')]))
      if (dups) issues <- c(issues, 'duplicate (study,treatment) rows')
    }
  }

  summ <- rbind(summ, data.frame(
    id = id,
    has_arm = isTRUE(has_arm),
    has_contrast = isTRUE(has_con),
    n_rows_arm = n_rows,
    n_treatments = n_treat,
    n_studies = n_stud,
    has_nodes = isTRUE(has_nodes),
    covariates = if (length(extra_cols)) paste(extra_cols, collapse=';') else '',
    issues = if (length(issues)) paste(issues, collapse='; ') else '',
    stringsAsFactors = FALSE
  ))
}

out_path <- file.path('data-raw','embedded_validation.csv')
dir.create('data-raw', showWarnings = FALSE)
utils::write.csv(summ, out_path, row.names = FALSE)
message('Wrote ', out_path, ' for ', nrow(summ), ' datasets.')
