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
cmp$diff <- abs(cmp$EUCOU_POS - cmp$EUCOU_POS.r)
p5 <- cmp[cmp$g105>=200 & !is.na(cmp$diff), ]
cat("Periode 5: maxtechq_ref =", max(refc$techq), " n =", nrow(p5), "\n")
bad <- p5[p5$diff>1e-3, c("g105","EUCOU_POS","EUCOU_POS.r","diff")]
cat("Abweichende Quartale (diff>1e-3):", nrow(bad), "\n")
print(bad, row.names=FALSE)
