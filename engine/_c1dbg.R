libpath <- "C:/Users/User/Documents/R/win-library/4.6"; .libPaths(c(libpath, .libPaths()))
suppressMessages({library(haven); library(dplyr)})
a18 <- "K:/DATEIEN/Data/PIP2022/_ARCHIV/_PIP_COLLECTION_v2018-02/03 pip_analysis/Framework"
source("K:/DATEIEN/Data/PIP2022/ASPM_WebApp/engine/lib_aspm_eu.R")
d   <- read_dta(file.path(a18,"datasets/pip_ts_aspm.dta")) |> as.data.frame()
gov <- read_dta(file.path(a18,"aspm_lr_quarterly.dta")) |> as.data.frame()
ref <- read_dta(file.path(a18,"aspm_lr_eu_quarterly.dta")) |> as.data.frame()
ext <- read_dta(file.path(a18,"datasets/external_data.dta")) |> as.data.frame()
eu <- eu_prep(d, "ja10f", gov, external=ext)
co <- eu_commission(eu, "ja10f")
refc <- ref |> group_by(techq) |> summarise(COMM_adj=mean(COMM_POS,na.rm=TRUE), EUCOU=mean(EUCOU_POS,na.rm=TRUE), .groups="drop")
refc$COMM_POS.r <- 2*refc$COMM_adj - refc$EUCOU
cmp <- co |> inner_join(refc, by=c("g105"="techq")); cmp$diff <- abs(cmp$COMM_POS-cmp$COMM_POS.r)
bad <- cmp[cmp$g105<=109 & cmp$diff>1e-3, c("g105","COMM_POS","COMM_POS.r","diff")]
cat("Periode1 abweichend:", nrow(bad), "von", sum(cmp$g105<=109), "; Bereich g105:", if(nrow(bad))paste(range(bad$g105),collapse="-") else "-", "\n")
print(head(bad,20), row.names=FALSE)
# Wieviele Kommissar-Parteien je fruehem Quartal? Und p625/p631/p645/p643 vorhanden?
cat("\np6xx nicht-NA gesamt: p625",sum(!is.na(d$p625)),"p631",sum(!is.na(d$p631)),"p643",sum(!is.na(d$p643)),"p645",sum(!is.na(d$p645)),"\n")
