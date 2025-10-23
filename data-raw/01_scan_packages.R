## Load helpers
source('R/registry_utils.R')
source('R/upstream.R')
source('R/upstream_scanner.R')

## Core packages to scan
core_pkgs <- c('gemtc','netmeta','multinma','MBNMAdose','pcnetmeta','nmaINLA','bnma','BUGSnet')

## Try BUGSnet via GitHub if missing
if (!requireNamespace('BUGSnet', quietly = TRUE)) {
  if (requireNamespace('remotes', quietly = TRUE)) try(remotes::install_github('audrey-b/BUGSnet'), silent = TRUE)
}

## Keep only installed ones (prevents errors on platforms without some deps)
core_pkgs <- core_pkgs[vapply(core_pkgs, requireNamespace, logical(1), quietly = TRUE)]

## Ensure registry exists and has the right schema
rp <- registry_path()
if (!file.exists(rp)) writeLines(paste(registry_target_cols(), collapse=','), rp)
cur <- if (file.exists(rp)) utils::read.csv(rp, stringsAsFactors = FALSE) else data.frame()
utils::write.csv(ensure_registry_schema(cur), rp, row.names = FALSE)

## Scan and append
scan_and_register_upstream(core_pkgs, install_if_missing = TRUE, write_now = TRUE)
