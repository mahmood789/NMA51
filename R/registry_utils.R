registry_target_cols <- function() c(
  'id','outcome','measure','source','source_url','license','citation','notes','package','object'
)

ensure_registry_schema <- function(df) {
  # Add any missing target columns as character NA; keep any extra columns too.
  tgt <- registry_target_cols()
  for (nm in setdiff(tgt, names(df))) df[[nm]] <- NA_character_
  # Reorder so target columns come first (extras kept at the end)
  df[ c(tgt, setdiff(names(df), tgt)) ]
}

registry_path <- function() {
  p <- system.file('registry','registry.csv', package = 'nmadatasets', mustWork = FALSE)
  if (!nzchar(p)) p <- file.path('inst','registry','registry.csv')
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  p
}
