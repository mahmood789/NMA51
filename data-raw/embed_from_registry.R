# Attempt to embed datasets listed in the registry by pulling from upstream packages
# Notes:
# - Requires upstream packages to be installed locally (no auto-install by default)
# - Uses heuristics to normalize to arm/contrast tables and writes to inst/extdata
# - Safe to rerun; existing files will be overwritten

source_if_exists <- function(f) if (file.exists(f)) source(f, local = FALSE)
source_if_exists('R/normalize_more.R')
source_if_exists('R/upstream.R')

ensure <- function(pkgs) {
  miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss)) message('Missing packages (not installing): ', paste(miss, collapse=', '))
}

reg_path <- if (file.exists('inst/registry/registry.csv')) 'inst/registry/registry.csv' else stop('No registry found')
reg <- utils::read.csv(reg_path, stringsAsFactors = FALSE)

# Determine which ids are already embedded (any of _studies/_contrast/.csv present)
ext <- if (dir.exists('inst/extdata')) 'inst/extdata' else { dir.create('inst/extdata', recursive = TRUE); 'inst/extdata' }
have <- unique(sub('_(nodes|studies|contrast)\\.csv$','', sub('\\.csv$','', list.files(ext, pattern='\\.csv$', full.names = FALSE))))
todo <- reg[!(reg$id %in% have) & !is.na(reg$package) & !is.na(reg$object), , drop = FALSE]

if (!nrow(todo)) { message('No new registry entries to embed.'); quit(save='no') }

ensure(unique(todo$package))

embedded <- character()
failed <- data.frame(id=character(), reason=character(), stringsAsFactors=FALSE)

for (i in seq_len(nrow(todo))) {
  id <- todo$id[i]; pkg <- todo$package[i]; obj <- todo$object[i]
  message('Embedding ', id, ' from ', pkg, '::', obj)
  ok <- try({
    raw <- get_upstream_raw(id)
    tables <- tryCatch(as_nma_tables(raw), error=function(e) NULL)
    if (is.null(tables)) stop('Normalization failed')
    write_embedded_local(id, tables, dir = ext)
    TRUE
  }, silent = TRUE)
  if (inherits(ok, 'try-error') || !isTRUE(ok)) {
    failed <- rbind(failed, data.frame(id=id, reason=as.character(ok), stringsAsFactors=FALSE))
  } else embedded <- c(embedded, id)
}

if (length(embedded)) message('Embedded: ', paste(embedded, collapse=', '))
if (nrow(failed)) {
  utils::write.csv(failed, file.path('data-raw','embed_failures.csv'), row.names = FALSE)
  message('Some embeddings failed; see data-raw/embed_failures.csv')
}
