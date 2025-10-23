# ---- helpers ----
.nz <- function(x) !is.null(x) && length(x) > 0
.has <- function(df, candidates) any(tolower(names(df)) %in% tolower(candidates))
.pick <- function(df, candidates) {
  nm <- intersect(tolower(names(df)), tolower(candidates)); if (!length(nm)) return(NULL)
  df[[ names(df)[ match(nm[1], tolower(names(df))) ] ]]
}
.std_arm <- function(df){
  # accepts many synonym columns and returns standardized arm table
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  # Keep a copy to preserve potential covariates/moderators
  orig <- df
  study <- .pick(df, c('study','studlab','trial','studyid','study_id'))
  trt   <- .pick(df, c('treat','treatment','trt','tx','t'))
  r     <- .pick(df, c('r','responders','event','events','y_event'))
  n     <- .pick(df, c('n','sampleSize','total','N','exposure','person_time','pt'))
  y     <- .pick(df, c('y','mean','response','effect'))
  sd    <- .pick(df, c('sd','stdev'))
  se    <- .pick(df, c('se','stderr'))
  # if only se given, convert to sd using n when possible
  if (.nz(se) && .nz(n) && !.nz(sd)) sd <- se * sqrt(n)
  base <- data.frame(study = study, treatment = trt, r = r, n = n, y = y, sd = sd,
                     stringsAsFactors = FALSE)
  if (!NROW(base) || (!.nz(r) && !(.nz(y) && (.nz(sd) || .nz(n)))))
    stop('Not an arm-level table I recognize: need (r,n) or (y,sd|se,n).')
  # Preserve additional columns as potential moderators (exclude core + obvious outcome cols)
  core <- c('study','studlab','trial','studyid','study_id','treat','treatment','trt','tx','t',
            'r','responders','event','events','y_event','n','sampleSize','total','N','y','mean','response','effect','sd','stdev','se','stderr','measure','outcome')
  extras <- setdiff(names(orig), core)
  if (length(extras)) {
    extra_df <- orig[extras]
    base <- cbind(base, extra_df)
  }
  base
}
.std_contrast <- function(df){
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  study <- .pick(df, c('study','studlab','trial','studyid','study_id','pair'))
  t1    <- .pick(df, c('treat1','t1','tx1','treatment1'))
  t2    <- .pick(df, c('treat2','t2','tx2','treatment2'))
  TE    <- .pick(df, c('TE','y','effect','logOR','logRR','mean_diff','SMD','HR','logHR'))
  seTE  <- .pick(df, c('seTE','se','stderr','selnOR','selogOR','se_logOR','se_logor','SE'))
  if (!.nz(t1) || !.nz(t2) || !.nz(TE) || !.nz(seTE))
    stop('Not a contrast-level table I recognize: need (treat1,treat2,TE,seTE).')
  data.frame(study = study %||% NA_character_, treat1 = t1, treat2 = t2, TE = TE, seTE = seTE,
             stringsAsFactors = FALSE)
}
.to_nodes <- function(trts){
  trts <- sort(unique(na.omit(as.character(trts))))
  data.frame(id = trts, name = trts, stringsAsFactors = FALSE)
}

# ---- main generic ----
as_nma_tables <- function(x) UseMethod('as_nma_tables')
as_nma_tables.default <- function(x){
  # try common shapes heuristically
  if (is.data.frame(x)){
    # BUGSnet contrast-like: differences + se.diffs with per-study treatment codes
    nms <- tolower(names(x))
    if (all(c('differences','se.diffs') %in% nms) && 'treatment' %in% nms && 'study' %in% nms) {
      df <- x; names(df) <- nms; df$.row <- seq_len(nrow(df))
      cons <- do.call(rbind, lapply(split(df, df$study), function(dd){
        if (nrow(dd) < 2) return(NULL)
        dd <- dd[order(dd$.row), ]
        i1 <- seq(1, nrow(dd)-1, by = 2); i2 <- i1 + 1
        data.frame(
          study = as.character(dd$study[i1]),
          treat1 = as.character(dd$treatment[i1]),
          treat2 = as.character(dd$treatment[i2]),
          TE = as.numeric(dd$differences[i2]),
          seTE = as.numeric(dd$`se.diffs`[i2]),
          stringsAsFactors = FALSE
        )
      }))
      if (!is.null(cons) && nrow(cons)) return(list(nodes = .to_nodes(c(cons$treat1, cons$treat2)), contrast = cons))
    }
    # Pairwise continuous means with SE -> contrast (2- or 3-arm rows)
    nms <- tolower(names(x))
    if (all(c('y1','y2','se1','se2','t1','t2') %in% nms)) {
      df <- x; names(df) <- nms
      build_rows <- function(row) {
        out <- list()
        if (!is.na(row$y1) && !is.na(row$y2) && !is.na(row$se1) && !is.na(row$se2) && !is.na(row$t1) && !is.na(row$t2)) {
          out[[length(out)+1]] <- data.frame(study = NA_character_,
                                             treat1 = as.character(row$t1),
                                             treat2 = as.character(row$t2),
                                             TE = as.numeric(row$y2) - as.numeric(row$y1),
                                             seTE = sqrt(as.numeric(row$se1)^2 + as.numeric(row$se2)^2),
                                             stringsAsFactors = FALSE)
        }
        if (all(c('y3','se3','t3') %in% nms)) {
          if (!is.na(row$y3) && !is.na(row$se3) && !is.na(row$t3)) {
            out[[length(out)+1]] <- data.frame(study = NA_character_,
                                               treat1 = as.character(row$t1),
                                               treat2 = as.character(row$t3),
                                               TE = as.numeric(row$y3) - as.numeric(row$y1),
                                               seTE = sqrt(as.numeric(row$se1)^2 + as.numeric(row$se3)^2),
                                               stringsAsFactors = FALSE)
          }
        }
        if (length(out)) do.call(rbind, out) else NULL
      }
      rows <- lapply(seq_len(nrow(df)), function(i) build_rows(df[i, , drop = FALSE]))
      rows <- Filter(Negate(is.null), rows)
      if (length(rows)) {
        con <- do.call(rbind, rows)
        return(list(nodes = .to_nodes(c(con$treat1, con$treat2)), contrast = con))
      }
    }
    # reset nms for subsequent checks
    nms <- tolower(names(x))
    # BUGSnet-style binary counts (e.g., diabetes/n columns)
    nms <- tolower(names(x))
    if (all(c('study','treatment','n') %in% nms) && any(c('diabetes','event','events','r') %in% nms)) {
      evcol <- intersect(c('diabetes','event','events','r'), nms)[1]
      df <- x; names(df) <- nms
      arm <- data.frame(
        study = as.character(df$study),
        treatment = as.character(df$treatment),
        r = as.numeric(df[[evcol]]),
        n = as.numeric(df$n),
        stringsAsFactors = FALSE
      )
      return(list(nodes = .to_nodes(arm$treatment), studies = arm))
    }
    # multinma contrast-like: y/se + trtn/trtc columns
    if (all(c('y','se') %in% nms) && any(c('trtn','treatn','t1') %in% nms) && any(c('trtc','treatc','t2') %in% nms)) {
      df <- x; names(df) <- nms
      t1 <- df[[ intersect(c('trtn','treatn','t1'), nms)[1] ]]
      t2 <- df[[ intersect(c('trtc','treatc','t2'), nms)[1] ]]
      stud <- if ('studyn' %in% nms) df$studyn else if ('studyc' %in% nms) df$studyc else if ('study' %in% nms) df$study else NA
      con <- data.frame(
        study = as.character(stud),
        treat1 = as.character(t1),
        treat2 = as.character(t2),
        TE = as.numeric(df$y),
        seTE = as.numeric(df$se),
        stringsAsFactors = FALSE
      )
      return(list(nodes = .to_nodes(c(con$treat1, con$treat2)), contrast = con))
    }
    if (.has(x, c('treat1','t1')) && .has(x, c('treat2','t2'))) {
      # First try contrast; if not contrast-shaped, try pairwise->arm mapping
      contrast <- tryCatch(.std_contrast(x), error = function(e) NULL)
      if (!is.null(contrast))
        return(list(nodes = .to_nodes(c(contrast$treat1, contrast$treat2)),
                    contrast = contrast))
      # fall back to pairwise arm-level reconstruction
      pw <- tryCatch(.pairwise_df_to_arm(x), error = function(e) NULL)
      if (!is.null(pw))
        return(list(nodes = .to_nodes(pw$treatment), studies = pw))
    } else if (.has(x, c('treat','treatment','trt'))) {
      arm <- .std_arm(x);
      return(list(nodes = .to_nodes(arm$treatment), studies = arm))
    }
  }
  if (is.list(x)){
    # bnma-style lists (binary)
    if (all(c('Outcomes','N','Study','Treat') %in% names(x))) {
      arm <- data.frame(
        study = as.character(x$Study),
        treatment = as.character(x$Treat),
        r = as.numeric(x$Outcomes),
        n = as.numeric(x$N),
        stringsAsFactors = FALSE
      )
      return(list(nodes = .to_nodes(arm$treatment), studies = arm))
    }
    # bnma-style lists (continuous: outcome + SE)
    if (all(c('Outcomes','SE','Treat','Study') %in% names(x))) {
      arm <- data.frame(
        study = as.character(x$Study),
        treatment = as.character(x$Treat),
        y = as.numeric(x$Outcomes),
        sd = as.numeric(x$SE),
        stringsAsFactors = FALSE
      )
      return(list(nodes = .to_nodes(arm$treatment), studies = arm))
    }
    # multinma-style
    if (!is.null(x$agd_contrast)) {
      contrast <- .std_contrast(x$agd_contrast);
      return(list(nodes = .to_nodes(c(contrast$treat1, contrast$treat2)),
                  contrast = contrast))
    }
    if (!is.null(x$agd_arm)) {
      arm <- .std_arm(x$agd_arm);
      return(list(nodes = .to_nodes(arm$treatment), studies = arm))
    }
    # BUGSnet / bnma / nmaINLA: first data.frame that looks like arm/contrast
    dfs <- Filter(is.data.frame, x)
    for (d in dfs){
      ok <- tryCatch(list(studies = .std_arm(d)), error = function(e) NULL)
      if (!is.null(ok)) return(list(nodes = .to_nodes(ok$studies$treatment), studies = ok$studies))
      ok2 <- tryCatch(list(contrast = .std_contrast(d)), error = function(e) NULL)
      if (!is.null(ok2)) return(list(nodes = .to_nodes(c(ok2$contrast$treat1, ok2$contrast$treat2)),
                                     contrast = ok2$contrast))
    }
    # Fallback: list with vector elements treat1/treat2 and TE/seTE directly
    nms <- tolower(names(x))
    pick_list <- function(nmset) {
      hits <- intersect(nmset, nms); if (!length(hits)) return(NULL)
      x[[ names(x)[ match(hits[1], nms) ] ]]
    }
    t1 <- pick_list(c('treat1','t1','tx1','treatment1'))
    t2 <- pick_list(c('treat2','t2','tx2','treatment2'))
    TE <- pick_list(c('te','y','effect','logor','logrr','mean_diff','smd','hr','loghr','lnor'))
    seTE <- pick_list(c('sete','se','stderr','selnor','selogor','se_logor','se_logOR','sete.w','se_te'))
    if (!is.null(t1) && !is.null(t2) && !is.null(TE) && !is.null(seTE)) {
      df <- data.frame(
        study = if (!is.null(pick_list(c('studlab','study','trial','author')))) as.character(pick_list(c('studlab','study','trial','author'))) else NA_character_,
        treat1 = as.character(t1),
        treat2 = as.character(t2),
        TE = as.numeric(TE),
        seTE = as.numeric(seTE),
        stringsAsFactors = FALSE
      )
      return(list(nodes = .to_nodes(c(df$treat1, df$treat2)), contrast = df))
    }
    # Fallback: list with arm-like vectors
    study <- pick_list(c('study','studlab','trial','id','author'))
    trt <- pick_list(c('treat','treatment','trt','tx','t'))
    r <- pick_list(c('r','responders','event','events','y_event'))
    n <- pick_list(c('n','samplesize','total','n_total','N'))
    y <- pick_list(c('y','mean','response','effect'))
    sd <- pick_list(c('sd','stdev'))
    se <- pick_list(c('se','stderr'))
    if (is.null(sd) && !is.null(se) && !is.null(n)) sd <- se * sqrt(as.numeric(n))
    if (!is.null(trt) && !is.null(study) && (!is.null(r) || (!is.null(y) && (!is.null(sd) || !is.null(n))))) {
      arm <- data.frame(study = as.character(study), treatment = as.character(trt), r = as.numeric(r), n = as.numeric(n), y = as.numeric(y), sd = as.numeric(sd), stringsAsFactors = FALSE)
      return(list(nodes = .to_nodes(arm$treatment), studies = arm))
    }
  }
  stop('Unknown upstream object structure (heuristics failed).')
}

# ---- gemtc ----
as_nma_tables.mtc.network <- function(x){
  if (.nz(x$data.ab)) {
    arm <- .std_arm(x$data.ab);
    return(list(nodes = .to_nodes(arm$treatment), studies = arm))
  }
  if (.nz(x$data.re)) {
    contrast <- .std_contrast(x$data.re);
    return(list(nodes = .to_nodes(c(contrast$treat1, contrast$treat2)),
                contrast = contrast))
  }
  stop('mtc.network data.ab/data.re not recognized')
}

# ---- netmeta object ----
as_nma_tables.netmeta <- function(x){
  # Expect typical slots: TE, seTE, treat1, treat2, studlab (optional)
  have <- all(c('TE','seTE','treat1','treat2') %in% names(x))
  if (!have) stop('netmeta object lacks TE/seTE/treat1/treat2')
  df <- data.frame(
    study = if ('studlab' %in% names(x)) as.character(x$studlab) else NA_character_,
    treat1 = as.character(x$treat1),
    treat2 = as.character(x$treat2),
    TE = as.numeric(x$TE),
    seTE = as.numeric(x$seTE),
    stringsAsFactors = FALSE
  )
  list(nodes = .to_nodes(c(df$treat1, df$treat2)), contrast = df)
}

# ---- helper: map pairwise wide (event1/n1/... y1/sd1/...) to long arm table ----
.pairwise_df_to_arm <- function(df){
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  nms <- tolower(names(df)); names(df) <- nms
  t1 <- (c('treat1','t1','treatment1','tx1')[c('treat1','t1','treatment1','tx1')%in%nms])[1]
  t2 <- (c('treat2','t2','treatment2','tx2')[c('treat2','t2','treatment2','tx2')%in%nms])[1]
  t3 <- (c('treat3','t3','treatment3','tx3')[c('treat3','t3','treatment3','tx3')%in%nms])[1]
  stud <- (c('studlab','study','trial','author','id')[c('studlab','study','trial','author','id')%in%nms])[1]
  # binary
  ev1 <- (c('event1','responders1','r1')[c('event1','responders1','r1')%in%nms])[1]
  ev2 <- (c('event2','responders2','r2')[c('event2','responders2','r2')%in%nms])[1]
  ev3 <- (c('event3','responders3','r3')[c('event3','responders3','r3')%in%nms])[1]
  n1  <- (c('n1','total1','sample1','n_1','n_01','e1','exposure1','pt1','E1')[c('n1','total1','sample1','n_1','n_01','e1','exposure1','pt1','E1')%in%nms])[1]
  n2  <- (c('n2','total2','sample2','n_2','n_02','e2','exposure2','pt2','E2')[c('n2','total2','sample2','n_2','n_02','e2','exposure2','pt2','E2')%in%nms])[1]
  n3  <- (c('n3','total3','sample3','n_3','n_03','e3','exposure3','pt3','E3')[c('n3','total3','sample3','n_3','n_03','e3','exposure3','pt3','E3')%in%nms])[1]
  # continuous
  y1  <- (c('y1','mean1')[c('y1','mean1')%in%nms])[1]
  y2  <- (c('y2','mean2')[c('y2','mean2')%in%nms])[1]
  y3  <- (c('y3','mean3')[c('y3','mean3')%in%nms])[1]
  sd1 <- (c('sd1','stdev1')[c('sd1','stdev1')%in%nms])[1]
  sd2 <- (c('sd2','stdev2')[c('sd2','stdev2')%in%nms])[1]
  sd3 <- (c('sd3','stdev3')[c('sd3','stdev3')%in%nms])[1]

  if (is.na(t1) || is.na(t2)) stop('pairwise mapping: missing treat1/2')

  S <- if (!is.na(stud)) as.character(df[[stud]]) else as.character(seq_len(nrow(df)))
  rows <- list()
  # detect mode: prefer binary if both event and n for first arm exist; else continuous
  bin_ok <- !is.na(ev1) && !is.na(n1)
  cont_ok <- !is.na(y1) && (!is.na(sd1) || !is.na(n1))
  if (!bin_ok && !cont_ok) stop('pairwise mapping: neither binary nor continuous columns found')

  if (bin_ok) {
    rows[[length(rows)+1]] <- data.frame(study=S, treatment=as.character(df[[t1]]), r=as.numeric(df[[ev1]]), n=as.numeric(df[[n1]]), stringsAsFactors=FALSE)
    rows[[length(rows)+1]] <- data.frame(study=S, treatment=as.character(df[[t2]]), r=as.numeric(df[[ev2]]), n=as.numeric(df[[n2]]), stringsAsFactors=FALSE)
    if (!is.na(t3) && !is.na(ev3) && !is.na(n3)) rows[[length(rows)+1]] <- data.frame(study=S, treatment=as.character(df[[t3]]), r=as.numeric(df[[ev3]]), n=as.numeric(df[[n3]]), stringsAsFactors=FALSE)
  } else {
    rows[[length(rows)+1]] <- data.frame(study=S, treatment=as.character(df[[t1]]), y=as.numeric(df[[y1]]), sd=as.numeric(df[[sd1]]), n=as.numeric(df[[n1]]), stringsAsFactors=FALSE)
    rows[[length(rows)+1]] <- data.frame(study=S, treatment=as.character(df[[t2]]), y=as.numeric(df[[y2]]), sd=as.numeric(df[[sd2]]), n=as.numeric(df[[n2]]), stringsAsFactors=FALSE)
    if (!is.na(t3) && !is.na(y3)) rows[[length(rows)+1]] <- data.frame(study=S, treatment=as.character(df[[t3]]), y=as.numeric(df[[y3]]), sd=as.numeric(df[[sd3]]), n=as.numeric(df[[n3]]), stringsAsFactors=FALSE)
  }
  do.call(rbind, rows)
}

# ---- writer (same filenames your embedder uses) ----
write_embedded_local <- function(id, tables, dir = file.path('inst','extdata')){
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  if (!is.null(tables$studies)) {
    utils::write.csv(tables$studies, file.path(dir, paste0(id, '_studies.csv')), row.names = FALSE)
    utils::write.csv(tables$nodes,   file.path(dir, paste0(id, '_nodes.csv')),   row.names = FALSE)
  } else if (!is.null(tables$contrast)) {
    utils::write.csv(tables$contrast, file.path(dir, paste0(id, '_contrast.csv')), row.names = FALSE)
    utils::write.csv(tables$nodes,    file.path(dir, paste0(id, '_nodes.csv')),    row.names = FALSE)
  } else stop('Nothing to write: need studies or contrast table.')
  invisible(TRUE)
}
