#' Scan packages for likely NMA datasets and append to registry
#' @param pkgs character vector of package names
#' @param install_if_missing logical, install from CRAN if needed
#' @param write_now logical, write to inst/registry/registry.csv
#' @export
scan_and_register_upstream <- function(pkgs, install_if_missing = TRUE, write_now = TRUE) {
  # local installer
  ensure <- function(pkgs) {
    miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
    if (length(miss) && install_if_missing) install.packages(miss)
  }
  ensure(pkgs)

  REGEX <- '(smok|depress|thrombo|diabet|parkin|diet|fat|atrial|fibril|blocker|statin|bcg|psoriasis|anxiety|triptan|migraine|stroke|pci|thromboly|vaccin|hba1c|hypertens)'
  KNOWN <- list(
    gemtc      = c('smoking','depression','dietfat','parkinson','blocker','atrialFibrillation','thrombolytic','hfPrevention','certolizumab'),
    netmeta    = c('smokingcessation','Franchini2012','Senn2013','Woods2010'),
    multinma   = c('smoking','diabetes','blocker','dietary_fat','Parkinsons','statins','BCG','thrombolytics','atrial_fibrillation','WBC','psoriasis','social_anxiety'),
    BUGSnet    = c('afib','diabetes','diabetes.sim','parkinsons','parkinsons_arm','thrombolytic'),
    MBNMAdose  = c('triptans'),
    pcnetmeta  = c('smoke','diabetes','dietaryfat','parkinson'),
    nmaINLA    = c('afib','stroke','psoriasis','thrombolysis'),
    bnma       = c('thrombolytic','PCI')
  )

  make_row <- function(pkg, obj, lic, cite) {
    tgt <- registry_target_cols()
    out <- as.list(structure(rep(NA_character_, length(tgt)), names = tgt))
    out$id        <- paste0(pkg,'__',obj)
    out$source    <- pkg
    out$source_url<- 'CRAN_or_GitHub'
    out$license   <- lic %||% NA_character_
    out$citation  <- cite %||% NA_character_
    out$notes     <- 'upstream_only; needs normalization'
    out$package   <- pkg
    out$object    <- obj
    as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
  }

  rows <- list()
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) next
    cat('→ scanning', pkg, '...\n')
    dd <- try(utils::data(package = pkg)$results, silent = TRUE)
    items <- character(0)
    if (!inherits(dd, 'try-error') && !is.null(dd) && NROW(dd)) {
      dd <- as.data.frame(dd, stringsAsFactors = FALSE)
      items <- unique(dd[['Item']])
      items <- items[!is.na(items)]
    }
    cand <- intersect(items, KNOWN[[pkg]] %||% character(0))
    more <- items[grepl(REGEX, items, ignore.case = TRUE)]
    objs <- unique(c(cand, more))
    if (!length(objs)) next
    desc <- tryCatch(utils::packageDescription(pkg), error = function(e) NULL)
    lic  <- if (!is.null(desc)) desc$License else NA_character_
    cite_txt <- tryCatch({
      ct <- utils::citation(pkg);
      paste0(capture.output(print(ct[1], style='text')), collapse=' ')
    }, error = function(e) NA_character_)
    for (obj in objs) rows[[length(rows)+1]] <- make_row(pkg, obj, lic, cite_txt)
  }

  add <- if (length(rows)) do.call(rbind, rows) else data.frame()
  if (!NROW(add)) { message('No candidate datasets found.'); return(invisible(tibble::tibble())) }

  rp <- registry_path()
  cur <- if (file.exists(rp)) utils::read.csv(rp, stringsAsFactors = FALSE) else data.frame()
  cur <- ensure_registry_schema(cur)
  add <- ensure_registry_schema(add)
  new <- add[!(add$id %in% cur$id), , drop = FALSE]
  all <- rbind(cur, new[, names(cur), drop = FALSE])
  if (write_now) utils::write.csv(all, rp, row.names = FALSE)
  tibble::as_tibble(new)
}

#' Search CRAN broadly for meta-analysis / NMA packages and scan them
#' @param install logical: install any new packages found before scanning
#' @export
search_cran_and_register <- function(install = FALSE) {
  ap <- utils::available.packages()
  df <- as.data.frame(ap, stringsAsFactors = FALSE)
  txt <- paste(df$Title, df$Description)
  hits <- grepl('network meta|network-meta|dose-response NMA|multi[- ]arm|multinma|BUGSnet|MBNMA|NMA|netmeta', txt, ignore.case = TRUE)
  pkgs <- setdiff(df$Package[hits], 'nmadatasets')
  if (install && length(pkgs)) {
    to_get <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
    if (length(to_get)) install.packages(to_get)
  }
  scan_and_register_upstream(pkgs, install_if_missing = FALSE, write_now = TRUE)
}
