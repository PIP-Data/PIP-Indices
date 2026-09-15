# Validierung gegen das KONSISTENTE v2022-Paar (Input 2022 + Output 2024-08).
libpath <- "C:/Users/User/Documents/R/win-library/4.6"; .libPaths(c(libpath, .libPaths()))
suppressMessages({library(haven); library(dplyr)})
fw <- "K:/DATEIEN/Data/PIP2022/_PIP_COLLECTION_v2022-03/03 pip_analysis/ASPM_Extended_Replication_2022/Framework"
source("K:/DATEIEN/Data/PIP2022/ASPM_WebApp/engine/lib_aspm.R")

d <- read_dta(file.path(fw,"datasets/pip_ts_aspm.dta")) |> as.data.frame()
ref <- read_dta(file.path(fw,"aspm_lr_quarterly.dta")) |> as.data.frame()
cat("Konsistenz: ja10f", round(range(d$ja10f,na.rm=TRUE),1),
    " | ref median1st", round(range(ref$median1st,na.rm=TRUE),1),
    " govmax", round(range(ref$govmax,na.rm=TRUE),1), "\n")
cat("Zeilen input", nrow(d), " ref", nrow(ref), "\n\n")

SPECS <- list(
  "40"=list(gov="seats",minority=TRUE,antisys=NULL,vp=c("gov")),
  "246"=list(gov="seats",minority=TRUE,antisys=NULL,vp=c("gov","pres")),
  "528"=list(gov="seats",minority=TRUE,antisys=NULL,vp=c("gov","2ch")),
  "578"=list(gov="seats",minority=TRUE,antisys=12951,vp=c("gov")),
  "752"=list(gov="seats",minority=TRUE,antisys=11951,vp=c("gov","2ch")),
  "56"=list(gov="seats",minority=TRUE,antisys=NULL,vp=c("gov","2ch")),
  "196"=list(gov="seats",minority=TRUE,antisys=NULL,vp=c("gov")),
  "442"=list(gov="seats",minority=TRUE,antisys=NULL,vp=c("gov")),
  "470"=list(gov="seats",minority=TRUE,antisys=NULL,vp=c("gov")),
  "100"=list(gov="seats",minority=TRUE,antisys=NULL,vp=c("gov","pres")),
  "203"=list(gov="seats",minority=TRUE,antisys=NULL,vp=c("gov")),
  "233"=list(gov="seats",minority=TRUE,antisys=NULL,vp=c("gov")),
  "348"=list(gov="seats",minority=TRUE,antisys=NULL,vp=c("gov")),
  "428"=list(gov="seats",minority=TRUE,antisys=NULL,vp=c("gov","pres")),
  "440"=list(gov="seats",minority=TRUE,antisys=NULL,vp=c("gov","pres")),
  "616"=list(gov="seats",minority=TRUE,antisys=NULL,vp=c("gov")),
  "642"=list(gov="seats",minority=TRUE,antisys=NULL,vp=c("gov","2ch")),
  "703"=list(gov="seats",minority=TRUE,antisys=NULL,vp=c("gov")),
  "705"=list(gov="seats",minority=TRUE,antisys=NULL,vp=c("gov"))
)
mine <- bind_rows(lapply(names(SPECS), function(k){
  dc <- d |> filter(g101==as.integer(k)); if(!nrow(dc)) return(NULL)
  estimate_country(dc, "ja10f", SPECS[[k]])
}))
cmp <- mine |> inner_join(ref |> select(iso,techq,median1st,median2nd,pres,GOV_POS,govmin,govmax,VP_RANGE,minoritygov),
                          by=c("g101"="iso","g105"="techq"), suffix=c("",".r"))
metric <- function(a,b){ ok<-!is.na(a)&!is.na(b); if(!sum(ok))return(c(NA,NA,NA))
  c(cor(a[ok],b[ok]), max(abs(a[ok]-b[ok])), mean(abs(a[ok]-b[ok])<1e-3)) }
cat("== Phase 1 (seats-Laender) R vs v2022-Referenz ==  n =", nrow(cmp), "\n")
for(v in c("median1st","median2nd","pres","GOV_POS","govmin","govmax","VP_RANGE","minoritygov")){
  m <- metric(cmp[[v]], cmp[[paste0(v,".r")]])
  cat(sprintf("  %-12s cor %.4f  maxdiff %.4g  exakt %.3f\n", v, m[1], m[2], m[3]))
}
