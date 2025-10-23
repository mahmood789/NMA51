
write_embedded <- function(id, pack, objname, norm) {
  dir.create("inst/extdata", recursive=TRUE, showWarnings=FALSE)
  arm_path    <- file.path("inst","extdata", paste0(id, ".csv"))
  con_path    <- file.path("inst","extdata", paste0(id, "_contrast.csv"))
  nodes_path  <- file.path("inst","extdata", paste0(id, "_nodes.csv"))
  studies_path<- file.path("inst","extdata", paste0(id, "_studies.csv"))
  meta_path   <- file.path("inst","extdata", paste0(id, "_meta.json"))

  if (!is.null(norm$arm))    utils::write.csv(norm$arm, arm_path, row.names = FALSE)
  if (!is.null(norm$contrast)) utils::write.csv(norm$contrast, con_path, row.names = FALSE)
  if (!is.null(norm$nodes))  utils::write.csv(norm$nodes, nodes_path, row.names = FALSE)
  if (!is.null(norm$studies)) utils::write.csv(norm$studies, studies_path, row.names = FALSE)

  meta <- list(
    id = id, source = pack, source_url = "CRAN_or_GitHub",
    license = NA_character_, citation = NA_character_,
    preferred_measure = norm$preferred_measure %||% "OR"
  )
  # try to inherit license/citation from registry (if present)
  if (file.exists(registry_path())) {
    reg <- tryCatch(utils::read.csv(registry_path(), stringsAsFactors=FALSE), error=function(e) data.frame())
    row <- reg[reg$id == id, , drop=FALSE]
    if (nrow(row)) { meta$license <- row$license[1]; meta$citation <- row$citation[1] }
  }
  jsonlite::write_json(meta, meta_path, auto_unbox = TRUE, pretty = TRUE)
  invisible(list(arm = if (exists("arm_path") && file.exists(arm_path)) arm_path,
                 contrast = if (exists("con_path") && file.exists(con_path)) con_path,
                 nodes = if (file.exists(nodes_path)) nodes_path,
                 meta = meta_path))
}

