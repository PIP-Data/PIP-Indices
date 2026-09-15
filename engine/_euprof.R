libpath <- "C:/Users/User/Documents/R/win-library/4.6"; .libPaths(c(libpath, .libPaths()))
suppressMessages({library(haven); library(dplyr)})
fw <- "K:/DATEIEN/Data/PIP2022/_PIP_COLLECTION_v2022-03/03 pip_analysis/ASPM_Extended_Replication_2022/Framework"
source("K:/DATEIEN/Data/PIP2022/ASPM_WebApp/engine/lib_aspm_eu.R")
d   <- as.data.frame(read_dta(file.path(fw,"datasets/pip_ts_aspm.dta")))
ext <- as.data.frame(read_dta(file.path(fw,"datasets/external_data.dta")))
ref <- as.data.frame(read_dta(file.path(fw,"datasets/swi_referenda.dta")))
cty <- read.csv("K:/DATEIEN/Data/PIP2022/ASPM_WebApp/app/config/countries.csv", colClasses="character")
specs <- setNames(as.list(cty$default_spec), cty$num)
govpos <- estimate_aspm(d, specs, ideo="ja10f", referenda=ref, verbose=FALSE)$quarterly
eu <- eu_prep(d, "ja10f", govpos, external=ext)
tm <- function(lbl, expr){ t<-Sys.time(); force(expr); cat(sprintf("  %-16s %.1f s\n", lbl, as.numeric(difftime(Sys.time(),t,units="secs")))) }
tm("eu_council",    eu_council(eu,"ja10f"))
tm("eu_commission", eu_commission(eu,"ja10f"))
tm("eu_councilofmin",eu_councilofmin(eu))
tm("eu_parliament", eu_parliament(eu,"ja10f"))
