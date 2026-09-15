# Validierung EP-Modell gegen konsistentes 2018-Paar
libpath <- "C:/Users/User/Documents/R/win-library/4.6"; .libPaths(c(libpath, .libPaths()))
suppressMessages({library(haven); library(dplyr)})
a18 <- "K:/DATEIEN/Data/PIP2022/_ARCHIV/_PIP_COLLECTION_v2018-02/03 pip_analysis/Framework"
source("K:/DATEIEN/Data/PIP2022/ASPM_WebApp/engine/lib_aspm_eu.R")
d   <- read_dta(file.path(a18,"datasets/pip_ts_aspm.dta")) |> as.data.frame()
gov <- read_dta(file.path(a18,"aspm_lr_quarterly.dta")) |> as.data.frame()
ref <- read_dta(file.path(a18,"aspm_lr_eu_quarterly.dta")) |> as.data.frame()
cat("2018 EU-Referenz Spalten:", paste(grep("POS|iso|techq",names(ref),value=TRUE),collapse=", "),"\n")

eu <- eu_prep(d, "ja10f", gov)
ep <- eu_parliament(eu, "ja10f", models=rep("median",5))
# Referenz EP_POS je techq (EU-weit, gleich ueber iso)
refep <- ref |> group_by(techq) |> summarise(EP_POS.r=mean(EP_POS,na.rm=TRUE), .groups="drop")
cmp <- ep |> inner_join(refep, by=c("g105"="techq"))
ok <- !is.na(cmp$EP_POS)&!is.na(cmp$EP_POS.r)
cat(sprintf("\n== EP_POS (weighted median) R vs 2018-Referenz ==  n=%d\n", sum(ok)))
cat(sprintf("  cor %.4f  maxdiff %.4g  exakt %.3f\n",
    cor(cmp$EP_POS[ok],cmp$EP_POS.r[ok]), max(abs(cmp$EP_POS[ok]-cmp$EP_POS.r[ok])), mean(abs(cmp$EP_POS[ok]-cmp$EP_POS.r[ok])<1e-3)))
