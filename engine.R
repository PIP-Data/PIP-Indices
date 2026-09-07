# ASPM-Engine in R — Portierung von techdata/est_*.do (Jahn 2016 Agenda Setting Power Model).
# Phase 1: Kammer-Mediane/Praesident (basics), Regierungsmodelle seats/uwmean, Minderheit (MCWC),
# Veto-Player-Range. Aggregation immer je (g101=Land, g105=Quartal). Validiert gegen aspm_lr_*.dta.
suppressMessages({library(dplyr)})

# Gewichteter Median wie Stata "collapse (median) [fweight=w]": nach Frequenzgewicht expandieren,
# dann Median (bei gerader Laenge Mittel der beiden mittleren). Nur ideo!=. und w>0.
wmedian <- function(x, w){
  keep <- !is.na(x) & !is.na(w) & w > 0
  x <- x[keep]; w <- round(w[keep])
  if(!length(x)) return(NA_real_)
  stats::median(rep(x, w))
}
`%||%` <- function(a,b) if(is.null(a)) b else a
ing <- function(v){ !is.na(v) & v==1 }   # "in government": p103==1 (sonst NA)

# --- basics: median1st, median2nd, pres je (g101,g105) ---
aspm_basics <- function(d, ideo){
  d$.ideo <- d[[ideo]]
  b <- d |> group_by(g101,g105) |> summarise(
        median1st = wmedian(.ideo, p303),
        median2nd = if(all(is.na(p402))) NA_real_ else wmedian(.ideo, p402),
        pres      = { v <- .ideo[ing(p502)]; if(length(v)&&any(!is.na(v))) mean(v,na.rm=TRUE) else NA_real_ },
        g106      = g106[1], .groups="drop")
  b$median1st[b$g106==9] <- NA; b$median2nd[b$g106==9] <- NA; b$pres[b$g106==9] <- NA
  b
}

# --- gov(seats): sitzgewichtetes Mittel der Regierungsparteien; Fallback median1st ---
gov_seats <- function(d, ideo, basics){
  d$.ideo <- d[[ideo]]
  g <- d |> group_by(g101,g105) |> summarise(
        newsumseat = sum(ifelse(ing(p103) & !is.na(.ideo), p303, 0), na.rm=TRUE),
        wsum       = sum(ifelse(ing(p103) & !is.na(.ideo), .ideo*p303, 0), na.rm=TRUE),
        .groups="drop") |>
        mutate(GOV_POS = ifelse(newsumseat>0, wsum/newsumseat, 0))
  g <- g |> left_join(basics |> select(g101,g105,median1st,g106), by=c("g101","g105"))
  g$GOV_POS[g$newsumseat==0] <- g$median1st[g$newsumseat==0]   # Expertenregierung
  g$GOV_POS[g$g106==9] <- NA
  g |> select(g101,g105,GOV_POS,newsumseat)
}

# --- gov(uwmean): ungewichtetes Mittel der Regierungsparteien ---
gov_uwmean <- function(d, ideo, basics){
  d$.ideo <- d[[ideo]]
  g <- d |> group_by(g101,g105) |> summarise(
        ncabinet = sum(ing(p103) & !is.na(.ideo)),
        GOV_POS  = { v <- .ideo[ing(p103) & !is.na(.ideo)]; if(length(v)) mean(v) else 0 },
        .groups="drop")
  g <- g |> left_join(basics |> select(g101,g105,median1st,g106), by=c("g101","g105"))
  g$GOV_POS[g$ncabinet==0] <- g$median1st[g$ncabinet==0]
  g$GOV_POS[g$g106==9] <- NA
  g |> rename(newsumseat=ncabinet) |> select(g101,g105,GOV_POS,newsumseat)
}

# --- gov gewichtet (seats=p303, cabmem=p107, portfolios=p110) ---
gov_weighted <- function(d, ideo, basics, wvar){
  d$.ideo <- d[[ideo]]; d$.w <- d[[wvar]]
  g <- d |> group_by(g101,g105) |> summarise(
        newsumseat = sum(ifelse(ing(p103) & !is.na(.ideo), .w, 0), na.rm=TRUE),
        wsum       = sum(ifelse(ing(p103) & !is.na(.ideo), .ideo*.w, 0), na.rm=TRUE),
        .groups="drop") |> mutate(GOV_POS = ifelse(newsumseat>0, wsum/newsumseat, 0))
  g <- g |> left_join(basics |> select(g101,g105,median1st,g106), by=c("g101","g105"))
  g$GOV_POS[g$newsumseat==0] <- g$median1st[g$newsumseat==0]
  g$GOV_POS[g$g106==9] <- NA
  g |> select(g101,g105,GOV_POS,newsumseat)
}

# --- gov(pm): Position des Premiers (p201==1); Fallback seats, dann median1st ---
gov_pm <- function(d, ideo, basics){
  d$.ideo <- d[[ideo]]
  g <- d |> group_by(g101,g105) |> summarise(
        pm  = { v <- .ideo[ing(p103) & !is.na(.ideo) & ing(p201)]; if(length(v)) max(v) else NA_real_ },
        newsumseat = sum(ifelse(ing(p103) & !is.na(.ideo), p303, 0), na.rm=TRUE),
        wsum       = sum(ifelse(ing(p103) & !is.na(.ideo), .ideo*p303, 0), na.rm=TRUE),
        .groups="drop") |> mutate(GOV_POS = ifelse(!is.na(pm), pm, ifelse(newsumseat>0, wsum/newsumseat, NA_real_)))
  g <- g |> left_join(basics |> select(g101,g105,median1st,g106), by=c("g101","g105"))
  g$GOV_POS[g$newsumseat==0 & !is.na(g$GOV_POS) & g$GOV_POS==0] <- g$median1st[g$newsumseat==0 & !is.na(g$GOV_POS) & g$GOV_POS==0]
  g$GOV_POS[g$g106==9] <- NA
  g |> select(g101,g105,GOV_POS,newsumseat)
}

# --- gov(unanimity): Mittelpunkt der beiden aeussersten Regierungsparteien ---
gov_unanimity <- function(d, ideo, basics){
  d$.ideo <- d[[ideo]]
  g <- d |> group_by(g101,g105) |> summarise(
        mn={v<-.ideo[ing(p103)&!is.na(.ideo)]; if(length(v))min(v) else NA_real_},
        mx={v<-.ideo[ing(p103)&!is.na(.ideo)]; if(length(v))max(v) else NA_real_},
        .groups="drop") |> mutate(GOV_POS = mn + (mx-mn)/2)
  g <- g |> left_join(basics |> select(g101,g105,median1st,g106), by=c("g101","g105"))
  g$GOV_POS[is.na(g$mn)] <- g$median1st[is.na(g$mn)]
  g$GOV_POS[g$g106==9] <- NA
  g$newsumseat <- ifelse(is.na(g$mn),0,1)
  g |> select(g101,g105,GOV_POS,newsumseat)
}

# --- gov(minister:portf): Mittel der Regierungsparteien mit Ressort; Kaskade; Fallback seats ---
# VEKTORISIERT (kein group_modify) fuer Performance (WebR).
gov_minister <- function(d, ideo, basics, portf){
  d$.ideo <- d[[ideo]]; d$.govok <- ing(d$p103) & !is.na(d$.ideo)
  sw <- d |> group_by(g101,g105) |> summarise(
          newsumseat=sum(ifelse(.govok,p303,0),na.rm=TRUE),
          wsum=sum(ifelse(.govok,.ideo*p303,0),na.rm=TRUE), .groups="drop") |>
        mutate(swpos=ifelse(newsumseat>0, wsum/newsumseat, NA_real_))
  g <- sw |> select(g101,g105,newsumseat,swpos)
  for(pf in portf){
    p <- d |> filter(.govok & !is.na(.data[[pf]]) & .data[[pf]]>0) |>
         group_by(g101,g105) |> summarise(.pfv=mean(.ideo), .groups="drop")
    g <- g |> left_join(p, by=c("g101","g105")); names(g)[names(g)==".pfv"] <- pf
  }
  g$GOV_POS <- NA_real_
  for(pf in portf) g$GOV_POS <- ifelse(is.na(g$GOV_POS), g[[pf]], g$GOV_POS)
  g$GOV_POS <- ifelse(is.na(g$GOV_POS), g$swpos, g$GOV_POS)
  g <- g |> left_join(basics |> select(g101,g105,median1st,g106), by=c("g101","g105"))
  fb <- g$newsumseat==0 & !is.na(g$GOV_POS) & g$GOV_POS==0
  g$GOV_POS[fb] <- g$median1st[fb]; g$GOV_POS[g$g106==9] <- NA
  g |> select(g101,g105,GOV_POS,newsumseat)
}

# --- gov(pmnegot): PM verhandelt mit Ressort-Partnern (oder Kabinett) in "inneren Koalitionen" ---
# VEKTORISIERT.
gov_pmnegot <- function(d, ideo, basics, portf){
  d$.ideo <- d[[ideo]]; d$.gov <- ing(d$p103) & !is.na(d$.ideo); d$.pm <- d$.gov & ing(d$p201)
  pm <- d |> group_by(g101,g105) |> summarise(
          pm_seats=sum(ifelse(.pm,p303,0),na.rm=TRUE),
          pm_pos=sum(ifelse(.pm,.ideo,0),na.rm=TRUE), .groups="drop")   # 0 wenn kein PM (wie Stata egen sum)
  dd <- d |> left_join(pm, by=c("g101","g105"))
  twoparty <- function(sub){ sub |> mutate(w=p303/(pm_seats+p303), z=(1-w)*pm_pos + w*.ideo) |>
                             group_by(g101,g105) |> summarise(.v=mean(z,na.rm=TRUE), .groups="drop") }
  g <- pm
  if(identical(portf,"cabinet")){
    p <- twoparty(dd |> filter(.gov & !ing(p201)))
    g <- g |> left_join(p, by=c("g101","g105"))
    g$GOV_POS <- ifelse(is.na(g$.v), g$pm_pos, g$.v); g$.v <- NULL
  } else {
    g$GOV_POS <- NA_real_
    for(pf in portf){
      p <- twoparty(dd |> filter(.gov & !is.na(.data[[pf]]) & .data[[pf]]>0))
      g <- g |> left_join(p, by=c("g101","g105"))
      g$GOV_POS <- ifelse(is.na(g$GOV_POS), g$.v, g$GOV_POS); g$.v <- NULL
    }
    g$GOV_POS <- ifelse(is.na(g$GOV_POS), g$pm_pos, g$GOV_POS)
  }
  g <- g |> left_join(basics |> select(g101,g105,median1st,g106), by=c("g101","g105"))
  fb <- g$pm_seats==0 & !is.na(g$GOV_POS) & g$GOV_POS==0
  g$GOV_POS[fb] <- g$median1st[fb]; g$GOV_POS[g$g106==9] <- NA
  g$newsumseat <- g$pm_seats
  g |> select(g101,g105,GOV_POS,newsumseat)
}

# --- minority (MCWC): ersetzt GOV_POS bei Minderheitsregierung; liefert minoritygov ---
# VEKTORISIERT: die kumulative Sitzsumme ist monoton -> below ist ein Praefix -> Kipp-Partei per
# lag(below) statt sequenzieller Schleife. Kein group_modify.
minority_mcwc <- function(d, ideo, gov, antisys=character(0)){
  d$.ideo <- d[[ideo]]
  d$GOV_POS <- setNames(gov$GOV_POS, paste(gov$g101,gov$g105))[paste(d$g101,d$g105)]
  d$.gov <- ing(d$p103) & !is.na(d$.ideo)
  d$.p303f <- ifelse(is.na(d$p303), 0, d$p303)
  d$.bad <- is.na(d$.ideo) | is.na(d$p118) | d$p118<2 | is.na(d$p303) | d$p303<1
  d$.pr <- abs(d$.ideo - d$GOV_POS)
  d$.pr[d$.gov] <- 0
  d$.pr[d$.bad] <- 999
  d$.pr[(d$p101 %in% antisys) & !d$.gov] <- 999
  d <- d |> group_by(g101,g105) |>
       mutate(abs_maj = sum(ifelse(!is.na(.ideo), .p303f, 0))/2,
              newgovseat = sum(ifelse(.gov, .p303f, 0))) |> ungroup()
  d <- d |> arrange(g101,g105,.pr,.p303f) |> group_by(g101,g105) |>
       mutate(cum = newgovseat + cumsum(ifelse(.gov,0,.p303f)),
              below = cum < abs_maj,
              dumm = .gov | below | dplyr::lag(below, default=FALSE)) |> ungroup()
  res <- d |> group_by(g101,g105) |> summarise(
           GOV_POS0 = GOV_POS[1], abs_maj=abs_maj[1], newgovseat=newgovseat[1], g106=g106[1],
           mtot = sum(.p303f[dumm]),
           gov_pos_mino = { w <- .p303f/sum(.p303f[dumm]); sum((ifelse(.gov, w*GOV_POS, ifelse(dumm & !.gov, w*.ideo, 0)))[dumm], na.rm=TRUE) },
           minogov = list(p101[dumm & !.gov]), .groups="drop")
  res$minoritygov <- ifelse(res$newgovseat>=res$abs_maj, 0, ifelse(res$newgovseat<res$abs_maj, 1, NA_real_))
  res$minoritygov[res$newgovseat==0 & res$gov_pos_mino==0] <- 0
  res$GOV_POS <- ifelse(!is.na(res$minoritygov) & res$minoritygov==1, res$gov_pos_mino, res$GOV_POS0)
  res$GOV_POS[res$g106==9] <- NA; res$minoritygov[res$g106==9] <- NA
  res |> select(g101,g105,GOV_POS,minoritygov,minogov)
}
aspm_g106 <- function(d) d |> group_by(g101,g105) |> summarise(g106=g106[1], .groups="drop")

# zeilenweises max/min ueber Spalten, NA ignorierend (leer -> NA)
rmax <- function(...){ M<-cbind(...); apply(M,1,function(r){r<-r[!is.na(r)]; if(length(r))max(r) else NA_real_}) }
rmin <- function(...){ M<-cbind(...); apply(M,1,function(r){r<-r[!is.na(r)]; if(length(r))min(r) else NA_real_}) }

# --- gov-min/max (mit MCWC-Stuetzparteien) + Traeger fuer VP-Formeln ---
gov_minmax <- function(d, ideo, basics, gov, minogov_map=NULL){
  d$.ideo <- d[[ideo]]; d$.inmcwc <- ing(d$p103)
  if(!is.null(minogov_map)){
    k <- paste(d$g101,d$g105)
    d$.inmcwc <- d$.inmcwc | mapply(function(kk,p) p %in% (minogov_map[[kk]] %||% integer(0)), k, d$p101)
  }
  gg <- d |> group_by(g101,g105) |> summarise(
        govmin = { v <- .ideo[.inmcwc]; if(length(v)&&any(!is.na(v))) min(v,na.rm=TRUE) else NA_real_ },
        govmax = { v <- .ideo[.inmcwc]; if(length(v)&&any(!is.na(v))) max(v,na.rm=TRUE) else NA_real_ },
        .groups="drop") |>
        left_join(gov |> select(g101,g105,GOV_POS), by=c("g101","g105")) |>
        left_join(basics |> select(g101,g105,median1st,median2nd,pres,g106), by=c("g101","g105"))
  fb <- is.na(gg$govmin) & !is.na(gg$GOV_POS) & !is.na(gg$median1st) & gg$GOV_POS==gg$median1st
  gg$govmin[fb] <- gg$median1st[fb]; gg$govmax[fb] <- gg$median1st[fb]
  gg
}

# --- EU-Position als Vetospieler-Punkt, aber nur solange das Land Mitglied ist (p601==1) ---
# Ausserhalb der Mitgliedschaft NA; NA erweitert den Range nicht (s. rmax/rmin).
eu_point <- function(d, g105, eu_pos){
  if(is.null(eu_pos) || !nrow(eu_pos)) return(rep(NA_real_, length(g105)))
  mem <- d |> group_by(g105) |> summarise(m=suppressWarnings(max(p601,na.rm=TRUE)), .groups="drop")
  mem$m[!is.finite(mem$m)] <- NA
  memv <- mem$m[match(g105, mem$g105)]
  pos  <- eu_pos$pos[match(g105, eu_pos$techq)]
  ifelse(!is.na(memv) & memv>0 & !is.na(pos), pos, NA_real_)
}

# --- vetoplayer: VP_RANGE nach vp-Liste (gov,1ch,2ch,pres). VP_RANGE_EU ist IMMER die gleiche
# Formel plus EU-Punkt - keine eigene vp-Option, sondern eine zweite, staendig mitgefuehrte Spalte,
# damit Nutzer die rein nationale und die EU-erweiterte Fassung direkt nebeneinander vergleichen koennen.
vetoplayer <- function(d, ideo, basics, gov, vp, minogov_map=NULL, eu_pos=NULL){
  gg <- gov_minmax(d, ideo, basics, gov, minogov_map)
  cm <- list(); cn <- list()
  if("gov" %in% vp){ cm$gov<-gg$govmax; cn$gov<-gg$govmin }
  if("1ch" %in% vp){ cm$a<-gg$median1st; cn$a<-gg$median1st }
  if("2ch" %in% vp){ cm$b<-gg$median2nd; cn$b<-gg$median2nd }
  if("pres"%in% vp){ cm$c<-gg$pres;      cn$c<-gg$pres }
  # Kein einziger Vetospieler gewaehlt -> eine Spanne ist nicht definiert, beide Spalten bleiben leer.
  # Frueher liefen rmax()/rmin() hier ohne Argumente (cbind() -> NULL) und die Berechnung brach ab.
  # Auch VP_RANGE_EU bleibt leer: eine "Spanne" aus dem EU-Punkt allein waere 0 und damit irrefuehrend.
  if(!length(cm)){
    gg$VP_RANGE <- NA_real_; gg$VP_RANGE_EU <- NA_real_
  } else {
    gg$VP_RANGE <- do.call(rmax,cm) - do.call(rmin,cn)
    ep <- eu_point(d, gg$g105, eu_pos); cm$eu<-ep; cn$eu<-ep
    gg$VP_RANGE_EU <- do.call(rmax,cm) - do.call(rmin,cn)
  }
  gg$govmin[gg$g106==9]<-NA; gg$govmax[gg$g106==9]<-NA
  gg$VP_RANGE[gg$g106==9]<-NA; gg$VP_RANGE_EU[gg$g106==9]<-NA
  gg |> select(g101,g105,govmin,govmax,VP_RANGE,VP_RANGE_EU)
}

# --- FRA-Special: Kohabitation (5. Republik). VP_RANGE_EU dieselbe Fallunterscheidung + EU-Punkt. ---
vp_special_fra <- function(d, ideo, basics, gov, minogov_map=NULL, eu_pos=NULL){
  gg <- gov_minmax(d, ideo, basics, gov, minogov_map); g5 <- gg$g105
  ep <- eu_point(d, g5, eu_pos)
  vr <- rep(NA_real_, nrow(gg)); vre <- rep(NA_real_, nrow(gg))
  i <- g5 <= -6                                                                          # IV. Rep: nur Regierung
  vr[i]  <- gg$govmax[i]-gg$govmin[i]
  vre[i] <- rmax(gg$govmax[i],ep[i]) - rmin(gg$govmin[i],ep[i])
  i <- g5 >  -6
  vr[i]  <- rmax(gg$govmax[i],gg$median2nd[i],gg$pres[i]) - rmin(gg$govmin[i],gg$median2nd[i],gg$pres[i])
  vre[i] <- rmax(gg$govmax[i],gg$median2nd[i],gg$pres[i],ep[i]) - rmin(gg$govmin[i],gg$median2nd[i],gg$pres[i],ep[i])
  coh <- (g5>=105&g5<=112)|(g5>=133&g5<=141)|(g5>=150&g5<=168)                          # Kohabitation
  vr[coh]  <- rmax(gg$govmax[coh],gg$median2nd[coh]) - rmin(gg$govmin[coh],gg$median2nd[coh])
  vre[coh] <- rmax(gg$govmax[coh],gg$median2nd[coh],ep[coh]) - rmin(gg$govmin[coh],gg$median2nd[coh],ep[coh])
  gg$VP_RANGE <- vr; gg$VP_RANGE_EU <- vre
  gg$govmin[gg$g106==9]<-NA; gg$govmax[gg$g106==9]<-NA
  gg$VP_RANGE[gg$g106==9]<-NA; gg$VP_RANGE_EU[gg$g106==9]<-NA
  gg |> select(g101,g105,govmin,govmax,VP_RANGE,VP_RANGE_EU)
}

# --- SWI-Special: Veto-Player inkl. Volksmedian (Referenden). Schweiz ist nie EU-Mitglied, daher
# ist VP_RANGE_EU hier stets identisch zu VP_RANGE (eu_point() liefert durchgehend NA). ---
vp_special_swi <- function(d, ideo, basics, gov, referenda, minogov_map=NULL, eu_pos=NULL){
  d$.ideo <- d[[ideo]]
  dd <- d |> left_join(referenda, by="id")
  vm <- dd |> group_by(g101,g105) |> summarise(
          countsum=suppressWarnings(max(countsum,na.rm=TRUE)),
          s=sum(volks*.ideo, na.rm=TRUE), .groups="drop") |>
        mutate(zs = ifelse(is.finite(countsum)&countsum!=0, s/countsum, NA_real_)) |> arrange(g105)
  # lineare Interpolation ueber g105, dann Rand fortschreiben (wie Stata ipolate + carry)
  idx <- which(!is.na(vm$zs))
  vm$vm_swi <- if(length(idx)>=2) approx(vm$g105[idx], vm$zs[idx], xout=vm$g105, rule=2)$y else vm$zs
  gg <- gov_minmax(d, ideo, basics, gov, minogov_map) |> left_join(vm |> select(g101,g105,vm_swi), by=c("g101","g105"))
  ep <- eu_point(d, gg$g105, eu_pos)
  gg$VP_RANGE <- rmax(gg$govmax,gg$median1st,gg$median2nd,gg$vm_swi) - rmin(gg$govmin,gg$median1st,gg$median2nd,gg$vm_swi)
  gg$VP_RANGE_EU <- rmax(gg$govmax,gg$median1st,gg$median2nd,gg$vm_swi,ep) - rmin(gg$govmin,gg$median1st,gg$median2nd,gg$vm_swi,ep)
  gg$govmin[gg$g106==9]<-NA; gg$govmax[gg$g106==9]<-NA
  gg$VP_RANGE[gg$g106==9]<-NA; gg$VP_RANGE_EU[gg$g106==9]<-NA
  gg |> select(g101,g105,govmin,govmax,VP_RANGE,VP_RANGE_EU,vm_swi)
}

# --- USA-Special: Agenda Setter (gov) + Veto-Player ---
gov_usa_special <- function(d, ideo, basics){
  d$.ideo <- d[[ideo]]; d <- d |> filter(p101!=61999)
  hs <- d |> group_by(g101,g105) |> group_modify(function(x, key){
    house <- { v<-x$.ideo[!is.na(x$p305)&x$p305>50]; if(length(v))max(v,na.rm=TRUE) else NA_real_ }
    dem <- { v<-x$.ideo[x$p101==61320]; if(length(v)&&any(!is.na(v)))max(v,na.rm=TRUE) else NA_real_ }
    rep <- { v<-x$.ideo[x$p101==61620]; if(length(v)&&any(!is.na(v)))max(v,na.rm=TRUE) else NA_real_ }
    g5 <- key$g105; thr <- if(g5<=60) 2/3 else 3/5
    # Senat: Majoritaetsfraktion (max p402), Filibuster-Kompromiss
    p402<-x$p402; p403<-x$p403[1]
    mj <- which(!is.na(p402) & p402==suppressWarnings(max(p402,na.rm=TRUE)))
    senate <- NA_real_
    if(length(mj) && is.finite(p403)){
      j<-mj[1]; maj_seats<-p402[j]; maj_pos<-x$.ideo[j]
      other <- if(x$p101[j]==61320) rep else if(x$p101[j]==61620) dem else NA_real_
      fbproof <- maj_seats > thr*p403
      raise <- max(0, ceiling(thr*p403 - maj_seats)); if(fbproof) raise<-0
      senate <- if(fbproof) maj_pos else (maj_seats*maj_pos + raise*other)/(thr*p403)
    }
    congress <- (house+senate)/2
    tibble(house=house, senate=senate, congress=congress)
  }) |> ungroup()
  gg <- hs |> left_join(basics |> select(g101,g105,pres,median1st,g106), by=c("g101","g105"))
  gg$aspi_usa <- 2/3*gg$pres + 1/3*gg$congress
  gg$GOV_POS <- gg$aspi_usa; gg$GOV_POS[gg$g106==9] <- NA
  gg$newsumseat <- ifelse(is.na(gg$GOV_POS),0,1)
  gg |> select(g101,g105,GOV_POS,house,senate,congress,aspi_usa,newsumseat)
}
vp_special_usa <- function(usa){
  vr <- rmax(usa$pres,usa$house,usa$senate) - rmin(usa$pres,usa$house,usa$senate)
  vint <- rmax(usa$pres,usa$senate) - rmin(usa$pres,usa$senate)
  # USA sind nie EU-Mitglied -> VP_RANGE_EU ist identisch zu VP_RANGE (kein eu_point() noetig).
  tibble(g101=usa$g101,g105=usa$g105,govmin=NA_real_,govmax=NA_real_,VP_RANGE=vr,VP_RANGE_EU=vr,vp_usa_int=vint)
}

# --- Config-String parsen (genau die Web-App-Syntax) -> spec-Liste ---
# z.B. "gov(pmnegot:p215,p213) minority(yes;antisys:53951) vp(gov,2ch)"
parse_spec <- function(s){
  gv <- sub(".*gov\\(([^)]*)\\).*","\\1", s)
  mn <- if(grepl("minority\\(",s)) sub(".*minority\\(([^)]*)\\).*","\\1", s) else "no"
  vp <- if(grepl("vp\\(",s)) sub(".*vp\\(([^)]*)\\).*","\\1", s) else "gov"
  portf <- NULL
  if(grepl("^minister:", gv)){ gov<-"minister"; portf<-trimws(strsplit(sub("minister:","",gv),",")[[1]]) }
  else if(grepl("^pmnegot:", gv)){ gov<-"pmnegot"; r<-sub("pmnegot:","",gv)
    portf<- if(trimws(r)=="cabinet") "cabinet" else trimws(strsplit(r,",")[[1]]) }
  else gov <- trimws(gv)
  minority <- grepl("yes", mn); antisys <- NULL
  if(grepl("antisys:", mn)){ a<-sub(".*antisys:","",mn); a<-trimws(a); if(a!="no") antisys<-as.numeric(strsplit(a,",")[[1]]) }
  list(gov=gov, portf=portf, minority=minority, antisys=antisys, vp=trimws(strsplit(vp,",")[[1]]))
}

# --- Voller ASPM-Lauf: specs = benannte Liste iso -> spec-Liste (oder Config-String) ---
# Rueckgabe: list(quarterly=, yearly=) mit iso,country,year,quarter,techq,eu + Schaetzungen.
estimate_aspm <- function(cmp, specs, ideo="ja10f", referenda=NULL, eu_pos=NULL, verbose=TRUE){
  cmp <- as.data.frame(cmp); cmp$g101 <- as.numeric(cmp$g101)
  est_cols <- c("median1st","median2nd","pres","GOV_POS","govmin","govmax","VP_RANGE","VP_RANGE_EU","minoritygov")
  parts <- list()
  for(k in names(specs)){
    iso <- as.numeric(k); dc <- cmp[cmp$g101==iso,]
    if(!nrow(dc)) next
    sp <- specs[[k]]; if(is.character(sp)) sp <- parse_spec(sp)
    e <- tryCatch(estimate_country(dc, ideo, sp, referenda=referenda, eu_pos=eu_pos),
                  error=function(err){ if(verbose) cat("  FEHLER", k, ":", conditionMessage(err), "\n"); NULL })
    if(!is.null(e)){ for(c in setdiff(est_cols,names(e))) e[[c]] <- NA_real_; parts[[k]] <- e[,c("g101","g105",est_cols)] }
    if(verbose) cat("  ", k, "ok\n")
  }
  combined <- bind_rows(parts)
  meta <- cmp |> group_by(g101,g105) |> summarise(country=g102[1], year=g103[1], quarter=g104[1],
            eu=suppressWarnings(max(p601,na.rm=TRUE)), .groups="drop")
  meta$eu[!is.finite(meta$eu)] <- NA
  q <- combined |> left_join(meta, by=c("g101","g105")) |>
       rename(iso=g101, techq=g105) |>
       select(iso,country,year,quarter,techq,eu, all_of(est_cols)) |> arrange(iso,techq)
  y <- q |> group_by(iso,year) |>
       summarise(country=country[1],
                 across(all_of(c("eu",est_cols)), ~mean(.x,na.rm=TRUE)), .groups="drop") |>
       mutate(eu=ifelse(eu>0,1,eu))
  list(quarterly=q, yearly=y)
}

# --- Orchestrator je Land: spec = list(gov=, minority=, antisys=, vp=, portf=) ---
# eu_pos: optional data.frame(techq, pos) mit der EU-Position je Quartal (fuer vp-Element "eu")
estimate_country <- function(dc, ideo, spec, referenda=NULL, eu_pos=NULL){
  iso <- as.numeric(dc$g101[1])
  basics <- aspm_basics(dc, ideo)

  # USA-Sonderfall: Agenda Setter (gov) [+ Minderheit] + spezieller Veto-Player
  if(identical(spec$gov,"special_aspm") && iso==840){
    usa <- gov_usa_special(dc, ideo, basics) |>
           left_join(basics |> select(g101,g105,pres), by=c("g101","g105"))
    gov <- usa |> select(g101,g105,GOV_POS,newsumseat)
    minoritygov <- NULL
    if(isTRUE(spec$minority)){
      mo <- minority_mcwc(dc, ideo, gov, antisys=spec$antisys %||% character(0))
      gov$GOV_POS <- mo$GOV_POS[match(paste(gov$g101,gov$g105), paste(mo$g101,mo$g105))]
      minoritygov <- mo |> select(g101,g105,minoritygov)
    }
    vpt <- vp_special_usa(usa)   # Veto-Player aus house/senate/pres (unabhaengig von GOV_POS)
    out <- basics |> select(g101,g105,median1st,median2nd,pres) |>
           left_join(gov |> select(g101,g105,GOV_POS), by=c("g101","g105")) |>
           left_join(vpt |> select(g101,g105,govmin,govmax,VP_RANGE,VP_RANGE_EU), by=c("g101","g105"))
    if(!is.null(minoritygov)) out <- out |> left_join(minoritygov, by=c("g101","g105")) else out$minoritygov <- 0
    return(out)
  }

  gov <- switch(spec$gov,
                seats      = gov_weighted(dc, ideo, basics, "p303"),
                cabmem     = gov_weighted(dc, ideo, basics, "p107"),
                portfolios = gov_weighted(dc, ideo, basics, "p110"),
                uwmean     = gov_uwmean(dc, ideo, basics),
                pm         = gov_pm(dc, ideo, basics),
                unanimity  = gov_unanimity(dc, ideo, basics),
                minister   = gov_minister(dc, ideo, basics, spec$portf),
                pmnegot    = gov_pmnegot(dc, ideo, basics, spec$portf),
                stop("gov model not yet ported: ", spec$gov))
  gov <- gov |> left_join(basics |> select(g101,g105,median1st), by=c("g101","g105"))
  minoritygov <- NULL; minogov_map <- NULL
  if(isTRUE(spec$minority)){
    mo <- minority_mcwc(dc, ideo, gov, antisys=spec$antisys %||% character(0))
    gov$GOV_POS <- mo$GOV_POS[match(paste(gov$g101,gov$g105), paste(mo$g101,mo$g105))]
    minoritygov <- mo |> select(g101,g105,minoritygov)
    # nur die Stuetzparteien zaehlen fuer govmin/max, und nur wo tatsaechlich Minderheit
    momin <- mo |> filter(!is.na(minoritygov) & minoritygov==1)
    minogov_map <- setNames(momin$minogov, paste(momin$g101,momin$g105))
  }
  if("special_aspm" %in% spec$vp){
    vpt <- if(iso==250) vp_special_fra(dc, ideo, basics, gov, minogov_map, eu_pos)
           else if(iso==756) vp_special_swi(dc, ideo, basics, gov, referenda, minogov_map, eu_pos)
           else vetoplayer(dc, ideo, basics, gov, "gov", minogov_map, eu_pos)
  } else vpt <- vetoplayer(dc, ideo, basics, gov, spec$vp, minogov_map, eu_pos)
  out <- basics |> select(g101,g105,median1st,median2nd,pres) |>
         left_join(gov |> select(g101,g105,GOV_POS), by=c("g101","g105")) |>
         left_join(vpt, by=c("g101","g105"))
  if(!is.null(minoritygov)) out <- out |> left_join(minoritygov, by=c("g101","g105")) else out$minoritygov <- NA
  out
}
