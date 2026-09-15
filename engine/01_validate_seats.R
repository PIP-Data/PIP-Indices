# Validierung Phase 1: seats-Laender (Default-Specs) vs. aspm_lr_quarterly.dta
libpath <- "C:/Users/User/Documents/R/win-library/4.6"; .libPaths(c(libpath, .libPaths()))
suppressMessages({library(haven); library(dplyr)})
fw <- "K:/DATEIEN/Data/PIP2022/ASPM Extended Replication/Release/Framework"
source("K:/DATEIEN/Data/PIP2022/ASPM_WebApp/engine/lib_aspm.R")

d <- read_dta(file.path(fw,"datasets/pip_ts_aspm.dta")) |> as.data.frame()
ref <- read_dta(file.path(fw,"aspm_lr_quarterly.dta")) |> as.data.frame()

SPECS <- list(
  "40" =list(gov="seats",minority=TRUE,antisys=NULL,      vp=c("gov")),
  "246"=list(gov="seats",minority=TRUE,antisys=NULL,      vp=c("gov","pres")),
  "528"=list(gov="seats",minority=TRUE,antisys=NULL,      vp=c("gov","2ch")),
  "578"=list(gov="seats",minority=TRUE,antisys=12951,     vp=c("gov")),
  "752"=list(gov="seats",minority=TRUE,antisys=11951,     vp=c("gov","2ch")),
  "56" =list(gov="seats",minority=TRUE,antisys=NULL,      vp=c("gov","2ch")),
  "196"=list(gov="seats",minority=TRUE,antisys=NULL,      vp=c("gov")),
  "442"=list(gov="seats",minority=TRUE,antisys=NULL,      vp=c("gov")),
  "470"=list(gov="seats",minority=TRUE,antisys=NULL,      vp=c("gov")),
  "100"=list(gov="seats",minority=TRUE,antisys=NULL,      vp=c("gov","pres")),
  "203"=list(gov="seats",minority=TRUE,antisys=NULL,      vp=c("gov")),
  "233"=list(gov="seats",minority=TRUE,antisys=NULL,      vp=c("gov")),
  "348"=list(gov="seats",minority=TRUE,antisys=NULL,      vp=c("gov")),
  "428"=list(gov="seats",minority=TRUE,antisys=NULL,      vp=c("gov","pres")),
  "440"=list(gov="seats",minority=TRUE,antisys=NULL,      vp=c("gov","pres")),
  "616"=list(gov="seats",minority=TRUE,antisys=NULL,      vp=c("gov")),
  "642"=list(gov="seats",minority=TRUE,antisys=NULL,      vp=c("gov","2ch")),
  "703"=list(gov="seats",minority=TRUE,antisys=NULL,      vp=c("gov")),
  "705"=list(gov="seats",minority=TRUE,antisys=NULL,      vp=c("gov"))
)
isos <- as.integer(names(SPECS))
res <- lapply(names(SPECS), function(k){
  iso <- as.integer(k); dc <- d |> filter(g101==iso)
  if(!nrow(dc)) return(NULL)
  estimate_country(dc, "ja10f", SPECS[[k]])
})
mine <- bind_rows(res)

cmp <- mine |> inner_join(ref |> select(iso,techq,median1st,median2nd,pres,GOV_POS,govmin,govmax,VP_RANGE,minoritygov),
                          by=c("g101"="iso","g105"="techq"), suffix=c("",".r"))
metric <- function(a,b){ ok<-!is.na(a)&!is.na(b); if(!sum(ok))return(c(NA,NA,NA))
  c(cor=cor(a[ok],b[ok]), maxdiff=max(abs(a[ok]-b[ok])), exact=mean(abs(a[ok]-b[ok])<1e-4)) }
cat("== Phase 1 (seats-Laender) R vs Referenz ==   n =", nrow(cmp), "\n")
for(v in c("median1st","median2nd","pres","GOV_POS","govmin","govmax","VP_RANGE","minoritygov")){
  m <- metric(cmp[[v]], cmp[[paste0(v,".r")]])
  cat(sprintf("  %-12s cor %.4f  maxdiff %.4g  exakt %.3f\n", v, m[1], m[2], m[3]))
}
