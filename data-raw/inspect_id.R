args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) stop('usage: Rscript data-raw/inspect_id.R <id>')
source('R/registry_utils.R')
source('R/upstream.R')
id <- args[[1]]
x <- get_upstream_raw(id)
cat('Class:', paste(class(x), collapse=';'), '\n')
if (is.list(x)) cat('Names:', paste(names(x), collapse=', '), '\n')
if (is.data.frame(x)) {
  cat('Data frame columns:', paste(names(x), collapse=', '), '\n')
  cat('Rows:', nrow(x), ' Cols:', ncol(x), '\n')
}
str(x, max.level = 1)
