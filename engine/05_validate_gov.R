# Validierung minister/pmnegot-Laender vs v2022-Referenz
libpath <- "C:/Users/User/Documents/R/win-library/4.6"; .libPaths(c(libpath, .libPaths()))
suppressMessages({library(haven); library(dplyr)})
fw <- "K:/DATEIEN/Data/PIP2022/_PIP_COLLECTION_v2022-03/03 pip_analysis/ASPM_Extended_Replication_2022/Framework"
source("K:/DATEIEN/Data/PIP2022/ASPM_WebApp/engine/lib_aspm.R")
d <- read_dta(file.path(fw,"datasets/pip_ts_aspm.dta")) |> as.data.frame()
ref <- read_dta(file.path(fw,"aspm_lr_quarterly.dta")) |> as.data.frame()
pf <- c("p215","p213")
SP <- list(
  "276"=list(gov="minister",portf=pf,minority=TRUE,antisys=NULL, vp=c("gov","2ch")),
  "380"=list(gov="minister",portf=pf,minority=TRUE,antisys=32710,vp=c("gov","2ch")),
  "300"=list(gov="minister",portf=pf,minority=TRUE,antisys=NULL, vp=c("gov")),
  "392"=list(gov="minister",portf=pf,minority=TRUE,antisys=NULL, vp=c("gov","2ch")),
  "124"=list(gov="pmnegot", portf=pf,minority=TRUE,antisys=NULL, vp=c("gov","2ch")),
  "36" =list(gov="pmnegot", portf=pf,minority=TRUE,antisys=NULL, vp=c("gov","2ch")),
  "724"=list(gov="pmnegot", portf=pf,minority=TRUE,antisys=NULL, vp=c("gov","2ch")),
  "826"=list(gov="pmnegot", portf=pf,minority=TRUE,antisys=NULL, vp=c("gov")),
  "554"=list(gov="pmnegot", portf=pf,minority=TRUE,antisys=NULL, vp=c("gov")),
  "372"=list(gov="pmnegot", portf=pf,minority=TRUE,antisys=53951,vp=c("gov")),
  "620"=list(gov="pmnegot", portf=pf,minority=TRUE,antisys=NULL, vp=c("gov","pres")),
  "208"=list(gov="pmnegot", portf=pf,minority=TRUE,antisys=13951,vp=c("gov","2ch"))
)
mine <- bind_rows(lapply(names(SP), function(k){
  dc <- d |> filter(g101==as.integer(k)); if(!nrow(dc)) return(NULL)
  tryCatch(estimate_country(dc,"ja10f",SP[[k]]), error=function(e){cat("ERR",k,conditionMessage(e),"\n");NULL})
}))
cmp <- mine |> inner_join(ref|>select(iso,techq,GOV_POS,govmin,govmax,VP_RANGE,minoritygov),
                          by=c("g101"="iso","g105"="techq"), suffix=c("",".r"))
metric <- function(a,b){ok<-!is.na(a)&!is.na(b);if(!sum(ok))return(c(NA,NA,NA));c(cor(a[ok],b[ok]),max(abs(a[ok]-b[ok])),mean(abs(a[ok]-b[ok])<1e-3))}
cat("== minister/pmnegot-Laender R vs Referenz ==  n =",nrow(cmp),"\n")
for(v in c("GOV_POS","govmin","govmax","VP_RANGE","minoritygov")){m<-metric(cmp[[v]],cmp[[paste0(v,".r")]]);cat(sprintf("  %-11s cor %.4f  maxdiff %.4g  exakt %.3f\n",v,m[1],m[2],m[3]))}
# je Land GOV_POS exakt
cat("\nGOV_POS exakt je Land:\n")
cmp |> group_by(g101) |> summarise(ex=mean(abs(GOV_POS-GOV_POS.r)<1e-3,na.rm=TRUE), .groups="drop") |> as.data.frame() |> print(row.names=FALSE)
