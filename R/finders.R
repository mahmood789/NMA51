 `%||%` <- function(a,b) if (!is.null(a)) a else b

registry_path <- function(){
  p <- system.file('registry','registry.csv', package='nmadatasets', mustWork=FALSE)
  if (!nzchar(p)) p <- file.path('inst','registry','registry.csv')
  dir.create(dirname(p), recursive=TRUE, showWarnings=FALSE); p
}
registry_target_cols <- function() c(
 'id','outcome','measure','source','source_url','license','citation','notes','package','object'
)
ensure_registry_schema <- function(df){
  tgt <- registry_target_cols()
  for (nm in setdiff(tgt, names(df))) df[[nm]] <- NA_character_
  df[ c(tgt, setdiff(names(df), tgt)) ]
}
register_remote <- function(id, url, title, license=NA_character_,
                            citation=NA_character_, notes='remote_only'){
  rp <- registry_path()
  cur <- if (file.exists(rp)) utils::read.csv(rp, stringsAsFactors=FALSE) else data.frame()
  cur <- ensure_registry_schema(cur)
  if (!(id %in% cur$id)) {
    row <- as.list(structure(rep(NA_character_, length(registry_target_cols())),
                             names = registry_target_cols()))
    row$id <- id; row$source <- 'remote'; row$source_url <- url;
    row$license <- license; row$citation <- citation; row$notes <- notes;
    cur <- rbind(cur, as.data.frame(row, stringsAsFactors=FALSE))
    utils::write.csv(cur, rp, row.names=FALSE)
  }
}

# ---------- small HTTP helper (httr if available; otherwise base) ----------
fetch_json <- function(url, headers=list()){
  if (requireNamespace('httr', quietly=TRUE)) {
    res <- httr::GET(url, httr::add_headers(.headers = headers),
                     httr::user_agent('nmadatasets-finders/0.1'))
    if (httr::http_error(res)) stop('HTTP error: ', httr::status_code(res))
    jsonlite::fromJSON(httr::content(res, as='text', encoding='UTF-8'), simplifyVector=TRUE)
  } else {
    con <- url(url); on.exit(close(con), add=TRUE)
    txt <- paste(readLines(con, warn=FALSE), collapse='')
    jsonlite::fromJSON(txt, simplifyVector=TRUE)
  }
}

# ========================== 1) Zenodo finder ===============================
zenodo_find_and_register <- function(
  queries = c('"network meta-analysis"', '"network meta analysis"', 'NMA "network"'),
  max_per_query = 60,
  file_ext_keep = c('csv','tsv','xlsx','rds','sav','txt','json')
){
  base <- 'https://zenodo.org/api/records'
  added <- 0L
  for (q in queries){
    page <- 1L; got <- 0L
    repeat {
      url <- paste0(base, '?q=', utils::URLencode(paste0(q,' AND (type:dataset)')),
                    '&size=', min(100, max_per_query - got), '&page=', page,
                    '&all_versions=true&sort=mostrecent')
      js <- tryCatch(fetch_json(url), error=function(e) NULL)
      if (is.null(js)) break
      hits <- js$hits$hits; if (is.null(hits) || !NROW(hits)) break
      for (i in seq_len(NROW(hits))){
        rec <- hits[i,]; md <- rec$metadata
        rid <- rec$id; title <- md$title %||% paste('Zenodo record', rid)
        lic <- md$license$id %||% md$license %||% NA_character_
        files <- rec$files
        ok <- TRUE
        if (!is.null(files) && NROW(files)){
          exts <- tolower(sub('.*\\.', '', files$filename))
          ok <- any(exts %in% tolower(file_ext_keep))
        }
        if (ok){
          id <- paste0('zenodo__r', rid)
          landing <- rec$links$html %||% paste0('https://zenodo.org/records/', rid)
          doi <- md$doi %||% md$prereserve_doi$doi %||% NA_character_
          cit <- paste0(title, if (!is.na(doi)) paste0(' (', doi, ')') else '')
          register_remote(id=id, url=landing, title=title, license=lic,
                          citation=cit, notes='remote_only; zenodo')
          added <- added + 1L
        }
      }
      got <- got + NROW(hits)
      if (got >= max_per_query) break
      page <- page + 1L
    }
  }
  message('Zenodo: added ~', added, ' remote candidates (if not already present).')
  invisible(added)
}

# ========================== 2) GitHub finder ===============================
github_find_and_register <- function(
  queries = c('"network meta-analysis" in:name,description,readme',
              '"network meta analysis" in:name,description,readme',
              '"NMA dataset" in:readme'),
  language = NULL,
  max_per_query = 60,
  token = Sys.getenv('GITHUB_TOKEN','')
){
  base <- 'https://api.github.com/search/repositories'
  hdrs <- c(Accept='application/vnd.github+json')
  if (nzchar(token)) hdrs <- c(Authorization=paste('Bearer', token), hdrs)
  added <- 0L
  for (q in queries){
    qfull <- q; if (!is.null(language)) qfull <- paste(qfull, paste0('language:', language))
    page <- 1L; got <- 0L
    repeat {
      url <- paste0(base, '?q=', utils::URLencode(qfull), '&per_page=100&page=', page, '&sort=updated')
      js <- tryCatch(fetch_json(url, headers = hdrs), error=function(e) NULL)
      if (is.null(js) || is.null(js$items) || !NROW(js$items)) break
      items <- js$items
      for (i in seq_len(NROW(items))){
        repo <- items[i,]
        keep <- grepl('data|dataset|analysis|paper|supplement',
                      tolower(paste(repo$name, repo$description)), perl=TRUE)
        if (keep){
          full <- repo$full_name
          id <- paste0('github__', gsub('[^A-Za-z0-9]+','_', full))
          lic <- repo$license$key %||% repo$license$name %||% NA_character_
          cit <- paste0(full, ' (GitHub). Stars: ', repo$stargazers_count)
          register_remote(id=id, url=repo$html_url, title=repo$full_name,
                          license=lic, citation=cit, notes='remote_only; github_repo')
          added <- added + 1L
        }
      }
      got <- got + NROW(items)
      if (got >= max_per_query || NROW(items) < 100) break
      page <- page + 1L
    }
  }
  message('GitHub: added ~', added, ' remote candidates (if not already present).')
  invisible(added)
}

# =========================== 3) CRAN packages ==============================
cran_find_and_register <- function(install = FALSE){
  ap <- utils::available.packages()
  df <- as.data.frame(ap, stringsAsFactors = FALSE)
  txt <- paste(df$Title, df$Description)
  hits <- grepl('network[- ]meta|network meta|multi[- ]arm|multinma|MBNMA|BUGSnet|netmeta|gemtc|dose[- ]response.*network',
                txt, ignore.case = TRUE)
  pkgs <- setdiff(df$Package[hits], 'nmadatasets')
  if (install && length(pkgs)) {
    miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly=TRUE)]
    if (length(miss)) install.packages(miss)
  }
  if (!exists('scan_and_register_upstream'))
    stop('scan_and_register_upstream() not found — source R/upstream_scanner.R first.')
  scan_and_register_upstream(pkgs, install_if_missing = FALSE, write_now = TRUE)
}
