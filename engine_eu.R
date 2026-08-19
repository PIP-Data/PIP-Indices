# ASPM EU-Engine (Portierung techdata/est_eu_*.do). Baut auf der Laender-GOV_POS auf.
# Aggregation je Quartal (g105) ueber den EU-weiten Pool (p601==1). Modellwahl je Vertragsperiode.
suppressMessages({library(dplyr)})
try(source("K:/DATEIEN/Data/PIP2022/ASPM_WebApp/engine/lib_aspm.R"), silent=TRUE)  # nativ: wmedian/%||%; in WebR ist engine.R schon geladen

wmean <- function(x,w){ k<-!is.na(x)&!is.na(w)&w>0; if(!any(k))return(NA_real_); sum(x[k]*w[k])/sum(w[k]) }
# Vertragsperiode aus techq (g105): tq(1987q2)=109, (1993q3)=134, (1999q1)=156, (2009q4)=199, (2010q1)=200
eu_period <- function(g5) ifelse(g5<=109,1L,ifelse(g5<=134,2L,ifelse(g5<=156,3L,ifelse(g5<=199,4L,5L))))
pick_period <- function(g5, models, tab){
  p <- eu_period(g5); vapply(seq_along(g5), function(i) tab[[ models[p[i]] ]][i], numeric(1))
}

# EU-Datensatz vorbereiten: nur Mitgliedsstaaten, GOV_POS + externe Gewichte anfuegen
eu_prep <- function(d, ideo, govpos, external=NULL){
  d <- d |> group_by(g101) |> mutate(.euk = suppressWarnings(mean(p601, na.rm=TRUE))) |> ungroup()
  d <- d[!is.na(d$.euk) & !is.nan(d$.euk), ]
  d$g101 <- as.numeric(d$g101)
  d <- d |> left_join(govpos |> select(iso,techq,GOV_POS), by=c("g101"="iso","g105"="techq"))
  if(!is.null(external)) d <- d |> left_join(external |> select(g101,g103,population,gdp_cur), by=c("g101","g103"))
  d
}

# --- Europaeischer Rat: Staatschefs, Machtgewichte (Population/GDP/Seniorität/Praesidentschaft) ---
PRESOVERPM_ISO <- c(250,470)  # FRA, MLT: Praesident statt PM
eu_council <- function(eu, ideo, models=rep("power",5), pmweight=c("population","gdp","seniority","presidency2"),
                       rotprespower="3:1:1", presoverpm=PRESOVERPM_ISO, debug=FALSE){
  eu$.ideo <- eu[[ideo]]
  # 1) je (g101,g105): Chief-Identitaet + headofstate + Traeger  (vektorisiert statt group_modify)
  eu$.isp  <- eu$g101 %in% presoverpm
  eu$.hpm  <- ifelse(!is.na(eu$p201)&eu$p201==1, eu$.ideo, NA_real_)      # PM-Position
  eu$.hpr  <- ifelse(!is.na(eu$p502)&eu$p502==1, eu$.ideo, NA_real_)      # Praesident-Position
  eu$.idpm <- ifelse(!is.na(eu$p201)&eu$p201==1, as.character(eu$p123), NA_character_)
  eu$.idpr <- ifelse(!is.na(eu$p502)&eu$p502==1, as.character(eu$p503), NA_character_)
  eu$.permv<- ifelse((eu$p101==21521 & eu$g105>=200 & eu$g105<=219) |
                     (eu$p101==92435 & eu$g105>=220 & eu$g105<=229), eu$.ideo, NA_real_)
  allna <- function(v) if(all(is.na(v))) NA_real_ else NA
  hs <- eu |> group_by(g101,g105) |> summarise(
    isp = .isp[1],
    hos_pm = if(all(is.na(.hpm))) NA_real_ else mean(.hpm, na.rm=TRUE),
    hos_pr = if(all(is.na(.hpr))) NA_real_ else mean(.hpr, na.rm=TRUE),
    id_pm  = { w<-which(!is.na(.idpm)); if(length(w)) .idpm[w[1]] else NA_character_ },
    id_pr  = { w<-which(!is.na(.idpr)); if(length(w)) .idpr[w[1]] else NA_character_ },
    GOV_POS = GOV_POS[1], population = population[1], gdp_cur = gdp_cur[1],
    perm_pres = if(all(is.na(.permv))) NA_real_ else max(.permv, na.rm=TRUE),
    p615 = { v<-p615[!is.na(p615)]; if(length(v)) v[1] else NA_real_ },
    p614 = { v<-p614[!is.na(p614)]; if(length(v)) v[1] else NA_real_ },
    member = as.integer(any(!is.na(p601)&p601==1)), .groups="drop")
  hs$headofstate <- ifelse(hs$isp, hs$hos_pr, hs$hos_pm)
  hs$id <- ifelse(hs$isp, hs$id_pr, hs$id_pm)
  hs$headofstate <- ifelse(is.na(hs$headofstate), hs$GOV_POS, hs$headofstate)
  # 2) Seniorität = Lauflaenge gleicher Chief (Nachname) je Land
  hs <- hs |> arrange(g101,g105)
  idc <- if(is.list(hs$id)) vapply(hs$id, function(z){z<-as.character(z); if(length(z)&&!is.na(z[1])) z[1] else NA_character_}, character(1)) else as.character(hs$id)
  hs$surname <- sub(" .*", "", idc, useBytes=TRUE)
  hs <- hs |> group_by(g101) |> mutate(
          brk = c(TRUE, surname[-1]!=surname[-length(surname)] | is.na(surname[-1]) | is.na(surname[-length(surname)])),
          experience = ave(seq_along(surname), cumsum(brk), FUN=seq_along)) |> ungroup()
  # Ab hier nur tatsaechliche Mitglieder je Quartal (Stata: drop if p601==. nach der Reduktion).
  # Senioritaet oben wurde ueber ALLE Quartale gezaehlt (wie in Stata vor der Reduktion).
  hs <- hs[hs$member==1, ]
  hs$sqrt_exp <- sqrt(hs$experience)
  hs$std_sqrt_exp <- hs$sqrt_exp / max(hs$sqrt_exp, na.rm=TRUE)
  # 3) EU-weite Standardpositionen + Gewichte je g105
  hs <- hs |> group_by(g105) |> mutate(
          sump=sum(population,na.rm=TRUE), sumg=sum(gdp_cur,na.rm=TRUE),
          std_pop=population/sump, std_gdp=gdp_cur/sumg) |> ungroup()
  perm_by <- hs |> group_by(g105) |> summarise(perm_pres=suppressWarnings(max(perm_pres,na.rm=TRUE)), .groups="drop")
  perm_by$perm_pres[!is.finite(perm_by$perm_pres)] <- NA
  # eucou_pres = headofstate des Praesidentschaftslandes (p614==1)
  agg <- hs |> group_by(g105) |> summarise(
    eucou_mean=mean(headofstate,na.rm=TRUE), eucou_median=stats::median(headofstate,na.rm=TRUE),
    mn=min(headofstate,na.rm=TRUE), mx=max(headofstate,na.rm=TRUE),
    eucou_pres={ v<-headofstate[!is.na(p614)&p614==1]; if(length(v))max(v,na.rm=TRUE) else NA_real_ },
    pos1=sum(std_pop*headofstate,na.rm=TRUE),
    pos2=sum(std_gdp*headofstate,na.rm=TRUE),
    pos3=sum(std_sqrt_exp*headofstate,na.rm=TRUE)/sum(std_sqrt_exp,na.rm=TRUE),
    .groups="drop") |> mutate(eucou_unanimity=mn+(mx-mn)/2)
  # presidency1 (pos4): Mittel ueber Nicht-Praesidentschaftslaender von (eucou_pres+headofstate)/2
  hs <- hs |> left_join(agg |> select(g105,eucou_pres), by="g105")
  pos4 <- hs |> mutate(z=ifelse(!is.na(p614)&p614==1, NA, (eucou_pres+headofstate)/2)) |>
          group_by(g105) |> summarise(pos4=mean(z,na.rm=TRUE), .groups="drop")
  # presidency2 (pos42): rotierende Praesidentschaft (Trio p615) + Permanent President, ab 2010
  rp <- as.numeric(strsplit(rotprespower,":")[[1]])  # current:succ:upcoming
  hs$zrot <- ifelse(hs$p615==3, rp[1], ifelse(hs$p615==2, rp[2], ifelse(hs$p615==1, rp[3], NA)))
  rot <- hs |> group_by(g105) |> summarise(
    zrot_total=sum(zrot,na.rm=TRUE),
    eucou_pres2=sum(headofstate*zrot,na.rm=TRUE)/sum(zrot,na.rm=TRUE), .groups="drop")
  rot$eucou_pres2[rot$g105<200] <- NA
  hs <- hs |> left_join(rot,by="g105") |> left_join(perm_by,by="g105",suffix=c("",".g"))
  pos42 <- hs |> mutate(z1=(eucou_pres2+perm_pres.g)/2, z5=ifelse(is.na(p615),(z1+headofstate)/2,NA)) |>
           group_by(g105) |> summarise(pos42=mean(z5,na.rm=TRUE), .groups="drop")
  P <- agg |> left_join(pos4,by="g105") |> left_join(pos42,by="g105")
  # eucou_power = rowmean der gewaehlten Gewichtspositionen
  cols <- c()
  if("population"%in%pmweight) cols<-c(cols,"pos1")
  if("gdp"%in%pmweight)        cols<-c(cols,"pos2")
  if("seniority"%in%pmweight)  cols<-c(cols,"pos3")
  if("presidency1"%in%pmweight)cols<-c(cols,"pos4")
  if("presidency2"%in%pmweight){ P$posp<-ifelse(is.na(P$pos42),P$pos4,P$pos42); cols<-c(cols,"posp") }
  P$eucou_power <- rowMeans(as.matrix(P[,cols,drop=FALSE]), na.rm=TRUE)
  P$EUCOU_POS <- pick_period(P$g105, models,
    within(P, { mean<-eucou_mean; median<-eucou_median; unanimity<-eucou_unanimity; power<-eucou_power }))
  if(debug) return(P)
  P |> select(g105, EUCOU_POS)
}

# --- Europaeische Kommission: Kommissar-Parteien (p625>=1). Modell "bargaining": Verhandlung zwischen
#     Praesident (p631>0) und Portfolio-Kommissar(en) (p645/p643), dann MCWC gewichtet mit p625. ---
eu_commission <- function(eu, ideo, portf=c("p645","p643"), models=rep("bargaining",5)){
  eu$.ideo <- eu[[ideo]]
  isc <- !is.na(eu$p601)&eu$p601==1 & !is.na(eu$p625)&eu$p625>=1
  eu$filterpos <- ifelse(isc & !is.na(eu$.ideo), eu$.ideo, ifelse(isc & is.na(eu$.ideo), eu$GOV_POS, NA_real_))
  eu$filter1   <- ifelse(isc & !is.na(eu$filterpos), eu$p625, NA_real_)
  cc <- eu[!is.na(eu$filter1), ]
  cc$.pf1 <- cc[[portf[1]]]; cc$.pf2 <- cc[[portf[2]]]; cc$.p631 <- cc$p631
  agg <- cc |> group_by(g105) |> summarise(
    comm_mean   = sum(filterpos*filter1,na.rm=TRUE)/sum(filter1,na.rm=TRUE),
    comm_median = wmedian(filterpos, filter1),
    .cmn=min(filterpos,na.rm=TRUE), .cmx=max(filterpos,na.rm=TRUE),
    comm_pres  = { v<-filterpos[!is.na(.p631)&.p631>0]; if(length(v)&&any(!is.na(v))) mean(v,na.rm=TRUE) else NA_real_ },
    .portf1    = { v<-filterpos[!is.na(.pf1)&.pf1>0]; if(length(v)&&any(!is.na(v))) mean(v,na.rm=TRUE) else NA_real_ },
    .portf2    = { v<-filterpos[!is.na(.pf2)&.pf2>0]; if(length(v)&&any(!is.na(v))) mean(v,na.rm=TRUE) else NA_real_ },
    .groups="drop")
  agg$comm_unanimity <- agg$.cmn + (agg$.cmx-agg$.cmn)/2
  agg$comm_portf <- dplyr::coalesce(agg$.portf1, agg$.portf2, agg$comm_mean)
  agg$comm_presportf <- (agg$comm_pres + agg$comm_portf)/2
  # MCWC ab comm_presportf, gewichtet mit filter1 (p625); Schwelle = halbe Kommissarszahl
  cc <- cc |> left_join(agg |> select(g105, comm_presportf), by="g105")
  cc <- cc |> mutate(.rg=abs(comm_presportf-filterpos)) |> arrange(g105,.rg) |>
        group_by(g105) |> mutate(
          .thr = sum(filter1,na.rm=TRUE)/2,
          .votes = 1 + cumsum(filter1) - dplyr::first(filter1),   # Stata: votes[1]=1, dann +filter1
          .bl = .votes <= .thr,
          .part = .bl | dplyr::lag(.bl, default=FALSE),
          comm_barg = mean(filterpos[.part], na.rm=TRUE)) |> ungroup()
  bg <- cc |> distinct(g105, comm_barg)
  agg <- agg |> left_join(bg, by="g105")
  colmap <- c(mean="comm_mean", median="comm_median", unanimity="comm_unanimity",
              portfolio="comm_portf", bargaining="comm_barg")
  agg$period <- eu_period(agg$g105); agg$COMM_POS <- NA_real_
  for(p in 1:5){ col<-colmap[[models[p]]]; sel<-agg$period==p; agg$COMM_POS[sel] <- agg[[col]][sel] }
  agg |> select(g105, COMM_POS)
}

# --- Ministerrat: je Quartal ueber Mitgliedsregierungen (GOV_POS). Stimmen p611, QMV-Schwelle p612,
#     Praesidentschaft p614, Trio p615. MCWC von der Praesidentschaft aus (qmv1_uw / qmv2_uw). ---
eu_councilofmin <- function(eu, models=c("unanimity","qmv1_uw","qmv1_uw","qmv1_uw","qmv2_uw"),
                            rotprespower="3:1:1"){
  cm <- eu[!is.na(eu$p601) & eu$p601==1, ]
  cm <- cm |> group_by(g101,g105) |> summarise(
          g103=g103[1], g104=g104[1], GOV_POS=GOV_POS[1],
          p611=suppressWarnings(max(p611,na.rm=TRUE)), p612=suppressWarnings(max(p612,na.rm=TRUE)),
          p614=as.integer(any(!is.na(p614)&p614==1)), p615=suppressWarnings(max(p615,na.rm=TRUE)),
          .groups="drop")
  for(v in c("p611","p612","p615")) cm[[v]][!is.finite(cm[[v]])] <- NA
  # Standardpositionen je g105
  cm <- cm |> group_by(g105) |> mutate(
          couofmin_mean_uw   = mean(GOV_POS,na.rm=TRUE),
          couofmin_median_uw = stats::median(GOV_POS,na.rm=TRUE),
          couofmin_mean      = wmean(GOV_POS,p611),
          couofmin_median    = wmedian(GOV_POS,p611),
          .cmin=min(GOV_POS,na.rm=TRUE), .cmax=max(GOV_POS,na.rm=TRUE),
          couofmin_pres = { v<-GOV_POS[!is.na(p614)&p614==1]; if(length(v)) v[1] else NA_real_ }) |> ungroup()
  cm$couofmin_unanimity <- cm$.cmin + (cm$.cmax-cm$.cmin)/2
  # QMV1 (feste Praesidentschaft): MCWC nach |Praesidentschaftsposition - GOV_POS|
  cm <- cm |> mutate(.r1=abs(couofmin_pres-GOV_POS)) |> arrange(g105,.r1,p611) |>
        group_by(g105) |> mutate(
          .cv=cumsum(ifelse(is.na(p611),0,p611)), .bl=.cv<p612,
          .p1=(!is.na(p614)&p614==1) | .bl | dplyr::lag(.bl,default=FALSE),
          couofmin_qmv1_uw = mean(GOV_POS[.p1],na.rm=TRUE),
          couofmin_qmv1    = sum((GOV_POS*p611)[.p1],na.rm=TRUE)/sum(p611[.p1],na.rm=TRUE)) |> ungroup()
  # QMV2 (rotierende Praesidentschaft-Trio ab 2010): Stimmen ab p615==3, alle Trio-Mitglieder dabei
  rp <- as.numeric(strsplit(rotprespower,":")[[1]])
  cm$.zrot <- ifelse(cm$p615==3,rp[1],ifelse(cm$p615==2,rp[2],ifelse(cm$p615==1,rp[3],NA)))
  r2 <- cm |> group_by(g105) |> summarise(couofmin_pres2=sum(GOV_POS*.zrot,na.rm=TRUE)/sum(.zrot,na.rm=TRUE),.groups="drop")
  cm <- cm |> left_join(r2,by="g105")
  cm$.r2 <- ifelse(!is.na(cm$.zrot),0,abs(cm$couofmin_pres2-cm$GOV_POS))
  cm <- cm |> arrange(g105,.r2,dplyr::desc(.zrot),p611) |> group_by(g105) |> mutate(
          .cv2=cumsum(ifelse(is.na(p611),0,p611)), .bl2=.cv2<p612,
          .p2=(!is.na(.zrot)) | .bl2 | dplyr::lag(.bl2,default=FALSE),
          couofmin_qmv2_uw = mean(GOV_POS[.p2],na.rm=TRUE),
          couofmin_qmv2    = sum((GOV_POS*p611)[.p2],na.rm=TRUE)/sum(p611[.p2],na.rm=TRUE)) |> ungroup()
  cm$couofmin_qmv2_uw[cm$g105<200] <- cm$couofmin_qmv1_uw[cm$g105<200]
  cm$couofmin_qmv2[cm$g105<200]    <- cm$couofmin_qmv1[cm$g105<200]
  # Modellwahl je Periode
  colmap <- c(mean="couofmin_mean",mean_uw="couofmin_mean_uw",median="couofmin_median",
              median_uw="couofmin_median_uw",unanimity="couofmin_unanimity",
              qmv1="couofmin_qmv1",qmv1_uw="couofmin_qmv1_uw",qmv2="couofmin_qmv2",qmv2_uw="couofmin_qmv2_uw")
  cm$period <- eu_period(cm$g105); cm$COUNCIL_POS <- NA_real_
  for(p in 1:5){ col<-colmap[[models[p]]]; sel<-cm$period==p; cm$COUNCIL_POS[sel] <- cm[[col]][sel] }
  cm |> distinct(g105, COUNCIL_POS)
}

# --- Orchestrator: komplette EU-Schaetzung (alle Institutionen + Zusammenbau) ---
# govpos = Laender-GOV_POS (aus estimate_aspm$quarterly, Spalten iso,techq,GOV_POS), external = population/gdp.
EU_DEFAULT <- list(
  council   = rep("power",5), pmweight = c("population","gdp","seniority","presidency2"),
  rotprespower = "3:1:1", presoverpm = c(250,470), principal = TRUE,
  commission = rep("bargaining",5), comm_portf = c("p645","p643"),
  councilofmin = c("unanimity","qmv1_uw","qmv1_uw","qmv1_uw","qmv2_uw"),
  euparl = rep("median",5), commonpos = "3:1", codec2 = "1:1")
estimate_eu <- function(d, ideo, govpos, external=NULL, spec=EU_DEFAULT){
  s <- modifyList(EU_DEFAULT, spec)
  eu <- eu_prep(d, ideo, govpos, external=external)
  eucou   <- eu_council(eu, ideo, models=s$council, pmweight=s$pmweight,
                        rotprespower=s$rotprespower, presoverpm=s$presoverpm)
  comm    <- eu_commission(eu, ideo, portf=s$comm_portf, models=s$commission)
  council <- eu_councilofmin(eu, models=s$councilofmin, rotprespower=s$rotprespower)
  ep      <- eu_parliament(eu, ideo, models=s$euparl)
  eu_assemble(eucou, comm, council, ep, principal=s$principal, commonpos=s$commonpos, codec2=s$codec2)
}

# --- Zusammenbau EU_POS aus den vier Institutionen (est_eu_legisprocess.do) ---
eu_assemble <- function(eucou, comm, council, ep, principal=TRUE, commonpos="3:1", codec2="1:1"){
  x <- eucou |> left_join(comm,by="g105") |> left_join(council,by="g105") |> left_join(ep,by="g105")
  if(principal){                                   # Rat als Prinzipal von Kommission & Ministerrat
    x$COMM_POS    <- (x$COMM_POS    + x$EUCOU_POS)/2
    x$COUNCIL_POS <- (x$COUNCIL_POS + x$EUCOU_POS)/2
  }
  cp<-as.numeric(strsplit(commonpos,":")[[1]]); pst<-cp[1]+cp[2]
  x$common_position <- (x$COUNCIL_POS*cp[1] + x$COMM_POS*cp[2])/pst
  x$second_reading  <- (x$common_position + x$EP_POS)/2
  x$check <- ifelse(abs(x$second_reading-x$COMM_POS) < abs(x$common_position-x$COMM_POS), 1,
             ifelse(abs(x$second_reading-x$COMM_POS) > abs(x$common_position-x$COMM_POS), 0, NA))
  x$common_position2 <- ifelse(x$check==1, x$second_reading, x$common_position)
  x$leg_consultation <- x$COUNCIL_POS
  x$leg_cooperation  <- (x$common_position2 + x$COUNCIL_POS)/2
  x$leg_codecision1  <- x$common_position
  c2<-as.numeric(strsplit(codec2,":")[[1]]); c2t<-c2[1]+c2[2]
  x$leg_codecision2  <- (x$COUNCIL_POS*c2[1] + x$EP_POS*c2[2])/c2t
  p <- eu_period(x$g105)
  x$EU_POS <- ifelse(p==1,x$leg_consultation, ifelse(p==2,x$leg_cooperation,
              ifelse(p==3,x$leg_codecision1, x$leg_codecision2)))
  x |> select(g105, EUCOU_POS, COMM_POS, COUNCIL_POS, EP_POS, EU_POS)
}

# --- Europaeisches Parlament: Pool aller Mitgliedsparteien mit EP-Sitzen (p604) ---
eu_parliament <- function(eu, ideo, models=rep("median",5)){
  eu$.ideo <- eu[[ideo]]
  eu$fp <- ifelse(!is.na(eu$p604), ifelse(!is.na(eu$.ideo), eu$.ideo, eu$GOV_POS), NA_real_)
  agg <- eu |> filter(!is.na(fp)) |> group_by(g105) |> summarise(
           mean_uw=mean(fp), median_uw=stats::median(fp), mn=min(fp), mx=max(fp),
           mean=wmean(fp,p604), median=wmedian(fp,p604), .groups="drop") |>
         mutate(unanimity = mn + (mx-mn)/2)
  agg$EP_POS <- pick_period(agg$g105, models, agg)
  agg |> select(g105, EP_POS)
}
