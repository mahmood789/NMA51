if (!exists('normalize_upstream')) normalize_upstream <- function(x, ...) UseMethod('normalize_upstream')

# Fallback: very forgiving heuristics for df/list/multinma/etc.
#' @export
normalize_upstream.default <- function(x, ...) {
  # data.frame: contrast or arm?
  if (is.data.frame(x)) {
    nms <- tolower(names(x))
    if (any(nms %in% c('treat1','t1')) && any(nms %in% c('treat2','t2'))) {
      contrast <- .std_contrast(x)
      return(list(nodes = .to_nodes(c(contrast$treat1, contrast$treat2)), contrast = contrast))
    }
    if (any(nms %in% c('treat','treatment','trt'))) {
      arm <- .std_arm(x)
      return(list(nodes = .to_nodes(arm$treatment), studies = arm))
    }
  }
  # list: multinma style or first recognizable df inside
  if (is.list(x)) {
    if (!is.null(x$agd_contrast)) {
      contrast <- .std_contrast(x$agd_contrast)
      return(list(nodes = .to_nodes(c(contrast$treat1, contrast$treat2)), contrast = contrast))
    }
    if (!is.null(x$agd_arm)) {
      arm <- .std_arm(x$agd_arm)
      return(list(nodes = .to_nodes(arm$treatment), studies = arm))
    }
    dfs <- Filter(is.data.frame, x)
    for (d in dfs) {
      arm_try <- try(.std_arm(d), silent = TRUE)
      if (!inherits(arm_try, 'try-error'))
        return(list(nodes = .to_nodes(arm_try$treatment), studies = arm_try))
      con_try <- try(.std_contrast(d), silent = TRUE)
      if (!inherits(con_try, 'try-error'))
        return(list(nodes = .to_nodes(c(con_try$treat1, con_try$treat2)), contrast = con_try))
    }
  }
  stop('Unknown upstream object structure')
}

# gemtc: access elements safely; avoid `$` surprises
#' @export
normalize_upstream.mtc.network <- function(x, ...) {
  ab <- x[['data.ab']]; re <- x[['data.re']]
  if (!is.null(ab) && is.data.frame(ab)) {
    arm <- .std_arm(ab)
    return(list(nodes = .to_nodes(arm$treatment), studies = arm))
  }
  if (!is.null(re) && is.data.frame(re)) {
    contrast <- .std_contrast(re)
    return(list(nodes = .to_nodes(c(contrast$treat1, contrast$treat2)), contrast = contrast))
  }
  stop('mtc.network data.ab not recognized')
}
