libpath <- "C:/Users/User/Documents/R/win-library/4.6"; .libPaths(c(libpath, .libPaths()))
suppressMessages({library(haven); library(dplyr)})
a18 <- "K:/DATEIEN/Data/PIP2022/_ARCHIV/_PIP_COLLECTION_v2018-02/03 pip_analysis/Framework"
source("K:/DATEIEN/Data/PIP2022/ASPM_WebApp/engine/lib_aspm_eu.R")
d   <- read_dta(file.path(a18,"datasets/pip_ts_aspm.dta")) |> as.data.frame()
gov <- read_dta(file.path(a18,"aspm_lr_quarterly.dta")) |> as.data.frame()
ext <- read_dta(file.path(a18,"datasets/external_data.dta")) |> as.data.frame()
eu <- eu_prep(d, "ja10f", gov, external=ext)
# van Rompuy CD&V (21521) und Tusk (92435): welche Zeilen/Werte?
for(pc in c(21521,92435)){
  s <- eu[eu$p101==pc & eu$g105 %in% c(210,214,218,222,226), c("g101","g105","p101","ja10f","p601")]
  cat("\n== p101", pc, "==\n"); print(as.data.frame(s), row.names=FALSE)
}
# Gibt es evtl. weitere Zeilen mit diesem p101 in anderen Laendern?
cat("\nLaender mit p101==21521:", paste(unique(eu$g101[eu$p101==21521]),collapse=","), "\n")
cat("Laender mit p101==92435:", paste(unique(eu$g101[eu$p101==92435]),collapse=","), "\n")
