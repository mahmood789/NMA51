#' Convert to a gemtc network
#' @export
as_gemtc <- function(x){
  stopifnot(inherits(x,'nma_data'))
  if (!requireNamespace('gemtc', quietly=TRUE)) stop("Package 'gemtc' is required for as_gemtc().")
  ab <- x$arm; needed <- c('study','treatment');
  if (!all(needed %in% names(ab))) stop('arm must contain: study, treatment')
  if (all(c('r','n')%in%names(ab))) {
    ab <- ab[,c('study','treatment','r','n')]; names(ab) <- c('study','treatment','responders','sampleSize')
  } else if (all(c('mean','sd','n')%in%names(ab))) {
    ab <- ab[,c('study','treatment','mean','sd','n')]; names(ab) <- c('study','treatment','mean','sd','sampleSize')
  } else if (all(c('r','exposure')%in%names(ab))) {
    ab <- ab[,c('study','treatment','r','exposure')]; names(ab) <- c('study','treatment','responders','exposure')
  } else stop('arm table missing outcome columns for gemtc.')
  tr <- NULL; if (!is.null(x$nodes)) tr <- data.frame(id = as.character(x$nodes$treatment), description = if ('description'%in%names(x$nodes)) as.character(x$nodes$description) else as.character(x$nodes$treatment), stringsAsFactors=FALSE)
  gemtc::mtc.network(data.ab = ab, treatments = tr) }

#' Convert to a netmeta object (arm- OR contrast-level)
#' @export
as_netmeta <- function(x){
  stopifnot(inherits(x,'nma_data'))
  if (!requireNamespace('netmeta', quietly=TRUE)) stop("Package 'netmeta' is required for as_netmeta().")
  if (!is.null(x$contrast)){
    df <- x$contrast; nms <- tolower(names(df)); names(df) <- nms;
    if (all(c('lnor','selnor')%in%nms)) { df$te <- df$lnor; df$sete <- df$selnor }
    if (!'studlab'%in%names(df)) df$studlab <- df$study %||% df$author %||% seq_len(nrow(df))
    sm <- x$meta$preferred_measure %||% 'OR'
    return(netmeta::netmeta(TE = df$te, seTE = df$sete, treat1 = df$treat1, treat2 = df$treat2, studlab = df$studlab, sm = sm, data = df))
  }
  # fallback: build from arm-level using pairwise()
  ab <- x$arm; sm <- x$meta$preferred_measure %||% if (all(c('r','n')%in%names(ab))) 'OR' else 'MD'
  if (all(c('r','n')%in%names(ab))) return(netmeta::pairwise(treat = treatment, event = r, n = n, studlab = study, data = ab, sm = sm))
  if (all(c('mean','sd','n')%in%names(ab))) return(netmeta::pairwise(treat = treatment, mean = mean, sd = sd, n = n, studlab = study, data = ab, sm = sm))
  stop('Neither contrast nor arm columns recognized for netmeta.') }
