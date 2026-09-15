libpath <- "C:/Users/User/Documents/R/win-library/4.6"; .libPaths(c(libpath, .libPaths()))
suppressMessages({library(haven); library(dplyr)})
a18 <- "K:/DATEIEN/Data/PIP2022/_ARCHIV/_PIP_COLLECTION_v2018-02/03 pip_analysis/Framework"
source("K:/DATEIEN/Data/PIP2022/ASPM_WebApp/engine/lib_aspm_eu.R")
d<-as.data.frame(read_dta(file.path(a18,"datasets/pip_ts_aspm.dta")))
gov<-as.data.frame(read_dta(file.path(a18,"aspm_lr_quarterly.dta")))
ref<-as.data.frame(read_dta(file.path(a18,"aspm_lr_eu_quarterly.dta")))
ext<-as.data.frame(read_dta(file.path(a18,"datasets/external_data.dta")))
out<-estimate_eu(d,"ja10f",gov,external=ext)
refq<-ref |> group_by(techq) |> summarise(EU_POS.r=mean(EU_POS,na.rm=TRUE),.groups="drop")
cmp<-out |> inner_join(refq,by=c("g105"="techq")); ok<-!is.na(cmp$EU_POS)&!is.na(cmp$EU_POS.r)
cat(sprintf("ESTIMATE_EU EU_POS cor %.5f exakt %.3f n %d cols %s\n",
  cor(cmp$EU_POS[ok],cmp$EU_POS.r[ok]),mean(abs(cmp$EU_POS[ok]-cmp$EU_POS.r[ok])<1e-3),sum(ok),paste(names(out),collapse=",")))
