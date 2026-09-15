# Validierung Sonderfaelle FRA/SWI/USA vs v2022-Referenz
libpath <- "C:/Users/User/Documents/R/win-library/4.6"; .libPaths(c(libpath, .libPaths()))
suppressMessages({library(haven); library(dplyr)})
fw <- "K:/DATEIEN/Data/PIP2022/_PIP_COLLECTION_v2022-03/03 pip_analysis/ASPM_Extended_Replication_2022/Framework"
source("K:/DATEIEN/Data/PIP2022/ASPM_WebApp/engine/lib_aspm.R")
d <- read_dta(file.path(fw,"datasets/pip_ts_aspm.dta")) |> as.data.frame()
ref <- read_dta(file.path(fw,"aspm_lr_quarterly.dta")) |> as.data.frame()
referenda <- read_dta(file.path(fw,"datasets/swi_referenda.dta")) |> as.data.frame()
pf <- c("p215","p213")
SP <- list(
  "250"=list(gov="pmnegot",portf=pf,minority=TRUE,antisys=31720,vp=c("special_aspm")),
  "756"=list(gov="seats",  minority=TRUE,antisys=NULL, vp=c("special_aspm")),
  "840"=list(gov="special_aspm", minority=TRUE, antisys=NULL, vp=c("special_aspm"))
)
metric <- function(a,b){ok<-!is.na(a)&!is.na(b);if(!sum(ok))return(c(NA,NA,NA));c(cor(a[ok],b[ok]),max(abs(a[ok]-b[ok])),mean(abs(a[ok]-b[ok])<1e-3))}
for(k in names(SP)){
  dc <- d |> filter(g101==as.integer(k))
  m <- tryCatch(estimate_country(dc,"ja10f",SP[[k]],referenda=referenda), error=function(e){cat("ERR",k,conditionMessage(e),"\n");NULL})
  if(is.null(m)) next
  cmp <- m |> inner_join(ref|>select(iso,techq,GOV_POS,VP_RANGE), by=c("g101"="iso","g105"="techq"),suffix=c("",".r"))
  cat(sprintf("\n== %s  (n=%d) ==\n", k, nrow(cmp)))
  for(v in c("GOV_POS","VP_RANGE")){mm<-metric(cmp[[v]],cmp[[paste0(v,".r")]]);cat(sprintf("  %-9s cor %.4f  maxdiff %.4g  exakt %.3f\n",v,mm[1],mm[2],mm[3]))}
}
