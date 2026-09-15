# Volle EU-Engine: alle vier Institutionen + Zusammenbau EU_POS, validiert gegen 2018-Archiv.
libpath <- "C:/Users/User/Documents/R/win-library/4.6"; .libPaths(c(libpath, .libPaths()))
suppressMessages({library(haven); library(dplyr)})
a18 <- "K:/DATEIEN/Data/PIP2022/_ARCHIV/_PIP_COLLECTION_v2018-02/03 pip_analysis/Framework"
source("K:/DATEIEN/Data/PIP2022/ASPM_WebApp/engine/lib_aspm_eu.R")
d   <- read_dta(file.path(a18,"datasets/pip_ts_aspm.dta")) |> as.data.frame()
gov <- read_dta(file.path(a18,"aspm_lr_quarterly.dta")) |> as.data.frame()
ref <- read_dta(file.path(a18,"aspm_lr_eu_quarterly.dta")) |> as.data.frame()
ext <- read_dta(file.path(a18,"datasets/external_data.dta")) |> as.data.frame()
eu <- eu_prep(d, "ja10f", gov, external=ext)

eucou   <- eu_council(eu, "ja10f")
comm    <- eu_commission(eu, "ja10f", portf=c("p645","p643"), models=rep("bargaining",5))
council <- eu_councilofmin(eu, models=c("unanimity","qmv1_uw","qmv1_uw","qmv1_uw","qmv2_uw"))
ep      <- eu_parliament(eu, "ja10f", models=rep("median",5))
out <- eu_assemble(eucou, comm, council, ep, principal=TRUE, commonpos="3:1", codec2="1:1")

refq <- ref |> group_by(techq) |> summarise(across(c(EUCOU_POS,COMM_POS,COUNCIL_POS,EP_POS,EU_POS), ~mean(.x,na.rm=TRUE)), .groups="drop")
cmp <- out |> inner_join(refq, by=c("g105"="techq"), suffix=c("",".r"))
metric <- function(a,b){ok<-!is.na(a)&!is.na(b); if(!sum(ok))return(c(NA,NA,NA)); c(cor(a[ok],b[ok]),max(abs(a[ok]-b[ok])),mean(abs(a[ok]-b[ok])<1e-3))}
cat("== VOLLE EU-ENGINE R vs 2018-Referenz ==  n =", nrow(cmp), "\n")
for(v in c("EUCOU_POS","COMM_POS","COUNCIL_POS","EP_POS","EU_POS")){
  m<-metric(cmp[[v]],cmp[[paste0(v,".r")]]); cat(sprintf("  %-12s cor %.4f  maxdiff %.4g  exakt %.3f\n",v,m[1],m[2],m[3]))
}
cat("\nEU_POS nach Periode:\n")
cmp$period <- eu_period(cmp$g105)
cmp |> filter(!is.na(EU_POS)&!is.na(EU_POS.r)) |> group_by(period) |>
  summarise(cor=cor(EU_POS,EU_POS.r), exakt=mean(abs(EU_POS-EU_POS.r)<1e-3), n=n(), .groups="drop") |> as.data.frame() |> print(row.names=FALSE)
