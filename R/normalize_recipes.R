nm_col <- function(df) tolower(gsub('[[:space:]]+','',gsub('[^A-Za-z0-9_]+','_',names(df))))

# if data.frame already looks like arm-level
to_arm_if_arm <- function(df){ nms <- nm_col(df); names(df) <- nms;
  if (!('study'%in%nms && 'treatment'%in%nms)) return(NULL)
  armish <- all(c('r','n')%in%nms) || all(c('mean','sd','n')%in%nms) || all(c('r','exposure')%in%nms)
  if (!armish) return(NULL)
  if (!'measure'%in%names(df)) df$measure <- if ('r'%in%names(df) && 'exposure'%in%names(df)) 'IRR' else if ('r'%in%names(df)) 'OR' else 'MD'
  if (!'outcome'%in%names(df)) df$outcome <- NA_character_
  nodes <- unique(df['treatment']); nodes$class <- NA_character_; nodes$description <- nodes$treatment
  list(arm=df, nodes=nodes, studies=NULL, preferred_measure=unique(df$measure)[1]) }

# pairwise (events/means) -> two-arm long
to_arm_from_pairwise <- function(df){ nms <- nm_col(df); names(df)<-nms;
  t1 <- (c('t1','treat1','treatment1','trt1')[c('t1','treat1','treatment1','trt1')%in%nms])[1]
  t2 <- (c('t2','treat2','treatment2','trt2')[c('t2','treat2','treatment2','trt2')%in%nms])[1]
  stud <- (c('study','studlab','trial','id','author')[c('study','studlab','trial','id','author')%in%nms])[1]
  has_bin  <- all(c('event1','n1','event2','n2')%in%nms)
  has_cont <- all(c('mean1','sd1','n1','mean2','sd2','n2')%in%nms)
  if (is.na(stud)||is.na(t1)||is.na(t2)||(!has_bin && !has_cont)) stop('pairwise format not recognized')
  S <- as.character(df[[stud]]); T1 <- as.character(df[[t1]]); T2 <- as.character(df[[t2]])
  if (has_bin){
    arm <- rbind(data.frame(study=S,treatment=T1,r=as.integer(df$event1),n=as.integer(df$n1),measure='OR',outcome=NA_character_),
                 data.frame(study=S,treatment=T2,r=as.integer(df$event2),n=as.integer(df$n2),measure='OR',outcome=NA_character_))
    pm <- 'OR'
  } else {
    arm <- rbind(data.frame(study=S,treatment=T1,mean=as.numeric(df$mean1),sd=as.numeric(df$sd1),n=as.integer(df$n1),measure='MD',outcome=NA_character_),
                 data.frame(study=S,treatment=T2,mean=as.numeric(df$mean2),sd=as.numeric(df$sd2),n=as.integer(df$n2),measure='MD',outcome=NA_character_))
    pm <- 'MD' }
  nodes <- unique(arm['treatment']); nodes$class <- NA_character_; nodes$description <- nodes$treatment
  list(arm=arm, nodes=nodes, studies=NULL, preferred_measure=pm) }

# contrast-level effects (e.g., lnOR/selnOR + treat1/treat2)
to_contrast_from_effects <- function(df){ nms <- nm_col(df); names(df)<-nms;
  have <- all(c('treat1','treat2')%in%nms) && (all(c('te','sete')%in%nms) || all(c('lnor','selnor')%in%nms))
  if (!have) return(NULL)
  if (!'te'%in%names(df) && 'lnor'%in%names(df)) df$te <- df$lnor
  if (!'sete'%in%names(df) && 'selnor'%in%names(df)) df$sete <- df$selnor
  if (!'studlab'%in%names(df)) df$studlab <- df$study %||% df$author %||% seq_len(nrow(df))
  # nodes from all treatments
  tr <- unique(c(as.character(df$treat1),as.character(df$treat2)))
  nodes <- data.frame(treatment=tr, class=NA_character_, description=tr, stringsAsFactors=FALSE)
  list(arm=NULL, contrast=df, nodes=nodes, studies=NULL, preferred_measure='OR') }

# gemtc-like object
to_arm_from_mtc <- function(obj){ ab <- obj$data.ab; tr <- obj$treatments;
  nodes <- if (is.data.frame(tr) && all(c('id','description')%in%names(tr)))
             data.frame(treatment=as.character(tr$id),class=NA_character_,description=as.character(tr$description),stringsAsFactors=FALSE) else NULL
  if (all(c('responders','exposure')%in%names(ab)))
    return(list(arm=data.frame(study=as.character(ab$study),treatment=as.character(ab$treatment),r=ab$responders,exposure=ab$exposure,measure='IRR',outcome=NA_character_),nodes=nodes,studies=obj$studies,preferred_measure='IRR'))
  if (all(c('responders','samplesize')%in%nm_col(ab))) { names(ab) <- nm_col(ab);
    return(list(arm=data.frame(study=as.character(ab$study),treatment=as.character(ab$treatment),r=ab$responders,n=ab$samplesize,measure='OR',outcome=NA_character_),nodes=nodes,studies=obj$studies,preferred_measure='OR'))}
  if (all(c('mean','sd','samplesize')%in%nm_col(ab))) { names(ab) <- nm_col(ab);
    return(list(arm=data.frame(study=as.character(ab$study),treatment=as.character(ab$treatment),mean=ab$mean,sd=ab$sd,n=ab$samplesize,measure='MD',outcome=NA_character_),nodes=nodes,studies=obj$studies,preferred_measure='MD'))}
  stop('mtc.network data.ab not recognized') }

normalize_upstream_object <- function(obj){
  if (inherits(obj,'mtc.network') || (is.list(obj) && all(c('data.ab','treatments')%in%names(obj)))) return(to_arm_from_mtc(obj))
  if (is.data.frame(obj)){
    out <- to_contrast_from_effects(obj); if (!is.null(out)) return(out)
    out <- to_arm_if_arm(obj);           if (!is.null(out)) return(out)
    out <- tryCatch(to_arm_from_pairwise(obj), error=function(e) NULL); if (!is.null(out)) return(out)
    stop('Unknown upstream object structure') }
  if (is.list(obj)){
    dfs <- Filter(is.data.frame, obj);
    for (df in dfs){ out <- tryCatch(normalize_upstream_object(df), error=function(e) NULL); if (!is.null(out)) return(out) }
    stop('list: no recognizable arm or contrast table found') }
  stop('Unsupported upstream type') }
