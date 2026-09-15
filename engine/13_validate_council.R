libpath <- "C:/Users/User/Documents/R/win-library/4.6"; .libPaths(c(libpath, .libPaths()))
suppressMessages({library(haven); library(dplyr)})
a18 <- "K:/DATEIEN/Data/PIP2022/_ARCHIV/_PIP_COLLECTION_v2018-02/03 pip_analysis/Framework"
source("K:/DATEIEN/Data/PIP2022/ASPM_WebApp/engine/lib_aspm_eu.R")
d   <- read_dta(file.path(a18,"datasets/pip_ts_aspm.dta")) |> as.data.frame()
gov <- read_dta(file.path(a18,"aspm_lr_quarterly.dta")) |> as.data.frame()
ref <- read_dta(file.path(a18,"aspm_lr_eu_quarterly.dta")) |> as.data.frame()
ext <- read_dta(file.path(a18,"datasets/external_data.dta")) |> as.data.frame()
eu <- eu_prep(d, "ja10f", gov, external=ext)
co <- eu_council(eu, "ja10f")
refc <- ref |> group_by(techq) |> summarise(EUCOU_POS.r=mean(EUCOU_POS,na.rm=TRUE), .groups="drop")
cmp <- co |> inner_join(refc, by=c("g105"="techq"))
ok <- !is.na(cmp$EUCOU_POS)&!is.na(cmp$EUCOU_POS.r)
cat(sprintf("== EUCOU_POS (power) R vs 2018-Referenz ==  n=%d\n", sum(ok)))
cat(sprintf("  cor %.4f  maxdiff %.4g  exakt %.3f\n",
    cor(cmp$EUCOU_POS[ok],cmp$EUCOU_POS.r[ok]), max(abs(cmp$EUCOU_POS[ok]-cmp$EUCOU_POS.r[ok])), mean(abs(cmp$EUCOU_POS[ok]-cmp$EUCOU_POS.r[ok])<1e-3)))
# nach Periode aufschluesseln
cmp$period <- eu_period(cmp$g105)
cmp[ok,] |> group_by(period) |> summarise(cor=cor(EUCOU_POS,EUCOU_POS.r), exakt=mean(abs(EUCOU_POS-EUCOU_POS.r)<1e-3), n=n(), .groups="drop") |> as.data.frame() |> print(row.names=FALSE)
