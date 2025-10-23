if (!exists('normalize_upstream')) normalize_upstream <- function(x, ...) UseMethod('normalize_upstream')

# ---- tiny utils -------------------------------------------------------------
.is_df <- function(x) inherits(x, 'data.frame')
.dfs_in <- function(x){
  out <- list()
  walk <- function(y){
    if (is.data.frame(y)) out[[length(out)+1]] <<- y
    else if (is.list(y)) lapply(y, walk)
  }
  walk(x); out
}

# ---------------- 1) gemtc: safe/raw extraction ------------------------------
#' @export
normalize_upstream.mtc.network <- function(x, ...) {
  xl <- unclass(x)                        # bypass any [[ methods
  ab <- xl[['data.ab', exact = TRUE]]
  re <- xl[['data.re', exact = TRUE]]
  # extremely defensive fallbacks
  if (!.is_df(ab) && !is.null(xl$data)) {
    ab <- xl$data[['data.ab', exact=TRUE]]
    re <- xl$data[['data.re', exact=TRUE]]
  }
  if (.is_df(ab)) {
    arm <- .std_arm(ab)
    return(list(nodes = .to_nodes(arm$treatment), studies = arm))
  }
  if (.is_df(re)) {
    contrast <- .std_contrast(re)
    return(list(nodes = .to_nodes(c(contrast$treat1, contrast$treat2)), contrast = contrast))
  }
  stop('mtc.network data.ab not recognized')
}

# ---- helper that tries to make sense of lists from many pkgs ----------------
.from_list_any <- function(x){
  # 1) multinma-style direct
  if (!is.null(x$agd_contrast) && .is_df(x$agd_contrast)) {
    con <- .std_contrast(x$agd_contrast)
    return(list(nodes = .to_nodes(c(con$treat1, con$treat2)), contrast = con))
  }
  if (!is.null(x$agd_arm) && .is_df(x$agd_arm)) {
    arm <- .std_arm(x$agd_arm)
    return(list(nodes = .to_nodes(arm$treatment), studies = arm))
  }
  # 2) bnma-style vector lists (no data.frame, just columns as vectors)
  need12 <- c('treat1','treat2')
  ycols  <- c('TE','y','effect','logOR','logRR','HR','logHR')
  secols <- c('seTE','se','SE','stderr')
  if (all(need12 %in% names(x)) && any(ycols %in% names(x)) && any(secols %in% names(x))) {
    t1 <- x[[need12[1]]]; t2 <- x[[need12[2]]]
    TE <- x[[ intersect(ycols, names(x))[1] ]]
    se <- x[[ intersect(secols, names(x))[1] ]]
    study <- x[['study']] %||% x[['studlab']] %||% NA_character_
    con <- data.frame(study = study, treat1 = t1, treat2 = t2, TE = TE, seTE = se,
                      stringsAsFactors = FALSE)
    return(list(nodes = .to_nodes(c(con$treat1, con$treat2)), contrast = con))
  }
  # 3) common wrappers: data/df/dat/arm/contrast, BUGSnet, nmaINLA, pcnetmeta, etc.
  candidates <- x[c('data','df','dat','dataset','arm','arm.data','contrast','contrast.data','agd','agd_arm','agd_contrast')]
  candidates <- candidates[!vapply(candidates, is.null, logical(1))]
  for (cand in candidates) {
    if (.is_df(cand)) {
      # try both shapes
      ok <- try(.std_arm(cand), silent = TRUE)
      if (!inherits(ok, 'try-error')) return(list(nodes = .to_nodes(ok$treatment), studies = ok))
      ok2 <- try(.std_contrast(cand), silent = TRUE)
      if (!inherits(ok2, 'try-error')) return(list(nodes = .to_nodes(c(ok2$treat1, ok2$treat2)), contrast = ok2))
    }
    if (is.list(cand)) {
      res <- try(.from_list_any(cand), silent = TRUE)
      if (!inherits(res, 'try-error')) return(res)
    }
  }
  # 4) brute force: recursively scan for first df that looks right
  dfs <- .dfs_in(x)
  for (d in dfs) {
    ok <- try(.std_arm(d), silent=TRUE)
    if (!inherits(ok, 'try-error')) return(list(nodes = .to_nodes(ok$treatment), studies = ok))
    ok2 <- try(.std_contrast(d), silent=TRUE)
    if (!inherits(ok2, 'try-error')) return(list(nodes = .to_nodes(c(ok2$treat1, ok2$treat2)), contrast = ok2))
  }
  stop('Unknown upstream object structure')
}

# ---------------- 2) default: dataframe OR any list ---------------------------
#' @export
normalize_upstream.default <- function(x, ...){
  if (.is_df(x)) {
    nms <- tolower(names(x))
    if (any(nms %in% c('treat1','t1')) && any(nms %in% c('treat2','t2'))) {
      con <- .std_contrast(x)
      return(list(nodes = .to_nodes(c(con$treat1, con$treat2)), contrast = con))
    }
    if (any(nms %in% c('treat','treatment','trt'))) {
      arm <- .std_arm(x)
      return(list(nodes = .to_nodes(arm$treatment), studies = arm))
    }
  }
  if (is.list(x)) return(.from_list_any(x))
  stop('Unknown upstream object structure')
}
