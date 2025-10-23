# Discover additional NMA datasets from CRAN, GitHub, and Zenodo and register them
# Requires network access; safe to rerun.

source('R/finders.R', local = TRUE)
source('R/upstream_scanner.R', local = TRUE)

# 1) Scan CRAN for NMA-related packages and register objects
search_cran_and_register(install = FALSE)

# 2) Register public GitHub repos likely to contain NMA datasets (remote-only)
github_find_and_register(max_per_query = 50)

# 3) Register Zenodo datasets with NMA keywords (remote-only)
zenodo_find_and_register(max_per_query = 50)

message('Updated registry at ', normalizePath(registry_path(), winslash = '/'))

