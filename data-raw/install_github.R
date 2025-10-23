if (!requireNamespace('remotes', quietly = TRUE)) install.packages('remotes', repos='https://cloud.r-project.org')

try_install <- function(repo) {
  cat('Trying GitHub repo: ', repo, ' ...\n', sep='')
  ok <- TRUE
  tryCatch(remotes::install_github(repo, upgrade = 'never'), error = function(e) { cat('Failed: ', conditionMessage(e), '\n', sep=''); ok <<- FALSE })
  ok
}

# Candidates for BUGSnet and nmaINLA
gh_targets <- list(
  BUGSnet = c('BUGSnet/BUGSnet', 'audrey-b/BUGSnet'),
  nmaINLA = c('esm-ispm-unil/nmaINLA', 'mathesong/nmaINLA', 'agailloty/nmaINLA')
)

installed <- character(); failed <- character()
for (pkg in names(gh_targets)) {
  if (requireNamespace(pkg, quietly = TRUE)) { installed <- c(installed, pkg); next }
  repos <- gh_targets[[pkg]]
  ok <- FALSE
  for (r in repos) { if (try_install(r)) { ok <- TRUE; break } }
  if (ok) installed <- c(installed, pkg) else failed <- c(failed, pkg)
}

cat('GitHub installed/available: ', paste(installed, collapse=', '), '\n', sep='')
if (length(failed)) cat('GitHub unavailable (skipped): ', paste(unique(failed), collapse=', '), '\n', sep='')

