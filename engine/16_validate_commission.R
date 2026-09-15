libpath <- "C:/Users/User/Documents/R/win-library/4.6"; .libPaths(c(libpath, .libPaths()))
suppressMessages({library(haven); library(dplyr)})
a18 <- "K:/DATEIEN/Data/PIP2022/_ARCHIV/_PIP_COLLECTION_v2018-02/03 pip_analysis/Framework"
source("K:/DATEIEN/Data/PIP2022/ASPM_WebApp/engine/lib_aspm_eu.R")
d   <- read_dta(file.path(a18,"datasets/pip_ts_aspm.dta")) |> as.data.frame()
gov <- read_dta(file.path(a18,"aspm_lr_quarterly.dta")) |> as.data.frame()
ref <- read_dta(file.path(a18,"aspm_lr_eu_quarterly.dta")) |> as.data.frame()
ext <- read_dta(file.path(a18,"datasets/external_data.dta")) |> as.data.frame()
eu <- eu_prep(d, "ja10f", gov, external=ext)
co <- eu_commission(eu, "ja10f", portf=c("p645","p643"), models=rep("bargaining",5))
# principal: Referenz-COMM = (roh_COMM+EUCOU)/2 -> roh zurueckrechnen
refc <- ref |> group_by(techq) |> summarise(COMM_adj=mean(COMM_POS,na.rm=TRUE), EUCOU=mean(EUCOU_POS,na.rm=TRUE), .groups="drop")
refc$COMM_POS.r <- 2*refc$COMM_adj - refc$EUCOU
cmp <- co |> inner_join(refc, by=c("g105"="techq"))
ok <- !is.na(cmp$COMM_POS)&!is.na(cmp$COMM_POS.r)
cat(sprintf("== COMM_POS (bargaining) R vs 2018-Referenz ==  n=%d\n", sum(ok)))
cat(sprintf("  cor %.4f  maxdiff %.4g  exakt %.3f\n",
    cor(cmp$COMM_POS[ok],cmp$COMM_POS.r[ok]), max(abs(cmp$COMM_POS[ok]-cmp$COMM_POS.r[ok])), mean(abs(cmp$COMM_POS[ok]-cmp$COMM_POS.r[ok])<1e-3)))
cmp$period <- eu_period(cmp$g105)
cmp[ok,] |> group_by(period) |> summarise(cor=cor(COMM_POS,COMM_POS.r), exakt=mean(abs(COMM_POS-COMM_POS.r)<1e-3), n=n(), .groups="drop") |> as.data.frame() |> print(row.names=FALSE)
