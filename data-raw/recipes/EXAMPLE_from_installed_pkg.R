# Template: pull a dataset from an installed package and normalize to the schema
pkg <- 'SOMEpkg'   # <- change me
obj <- 'SOMEdata'  # <- change me

if (requireNamespace(pkg, quietly = TRUE)) {
  dat <- get(obj, envir = asNamespace(pkg))
  # TODO: map dat -> arm-level with columns study, treatment, r/n OR mean/sd/n
  arm <- tibble::tibble(
    study = dat$study,
    treatment = dat$treatment,
    r = dat$r, n = dat$n,
    measure = 'OR',
    outcome = 'MACE'
  )
  nodes <- arm |> dplyr::distinct(treatment) |> dplyr::mutate(class = NA_character_, description = NA_character_)
  meta <- list(
    id = 'example_bin',
    source = pkg, source_url = NA_character_,
    license = 'see upstream', citation = 'Add full citation',
    preferred_measure = 'OR'
  )
  dir.create('inst/extdata', showWarnings = FALSE, recursive = TRUE)
  utils::write.csv(arm, 'inst/extdata/example_bin.csv', row.names = FALSE)
  utils::write.csv(nodes, 'inst/extdata/example_bin_nodes.csv', row.names = FALSE)
  jsonlite::write_json(meta, 'inst/extdata/example_bin_meta.json', auto_unbox = TRUE, pretty = TRUE)
  reg <- 'inst/registry/registry.csv'
  cur <- if (file.exists(reg)) utils::read.csv(reg, stringsAsFactors = FALSE) else data.frame()
  add <- data.frame(id = 'example_bin', outcome = 'MACE', measure = 'OR',
                    source = pkg, source_url = NA, license = 'see upstream',
                    citation = 'Add full citation', notes = '', stringsAsFactors = FALSE)
  utils::write.csv(dplyr::bind_rows(cur, dplyr::anti_join(add, cur, by = 'id')), reg, row.names = FALSE)
}
