libpath <- "C:/Users/User/Documents/R/win-library/4.6"; .libPaths(c(libpath, .libPaths()))
suppressMessages({library(haven); library(dplyr)})
a18 <- "K:/DATEIEN/Data/PIP2022/_ARCHIV/_PIP_COLLECTION_v2018-02/03 pip_analysis/Framework"
source("K:/DATEIEN/Data/PIP2022/ASPM_WebApp/engine/lib_aspm_eu.R")
d   <- read_dta(file.path(a18,"datasets/pip_ts_aspm.dta")) |> as.data.frame()
gov <- read_dta(file.path(a18,"aspm_lr_quarterly.dta")) |> as.data.frame()
ref <- read_dta(file.path(a18,"aspm_lr_eu_quarterly.dta")) |> as.data.frame()
ext <- read_dta(file.path(a18,"datasets/external_data.dta")) |> as.data.frame()
eu <- eu_prep(d, "ja10f", gov, external=ext)
P <- eu_council(eu, "ja10f", debug=TRUE)
refc <- ref |> group_by(techq) |> summarise(EUCOU=mean(EUCOU_POS,na.rm=TRUE), .groups="drop")
m <- P |> inner_join(refc, by=c("g105"="techq")) |> filter(!is.na(EUCOU))
m$posp <- ifelse(is.na(m$pos42), m$pos4, m$pos42)
# implizierte Praesidentschaftskomponente aus der Referenz
m$posp_soll <- 4*m$EUCOU - m$pos1 - m$pos2 - m$pos3
ac <- function(a,b){ok<-!is.na(a)&!is.na(b);c(cor=cor(a[ok],b[ok]),maxd=max(abs(a[ok]-b[ok])),ex=mean(abs(a[ok]-b[ok])<1e-3))}
cat("posp (mein) vs posp_soll:      "); print(round(ac(m$posp,m$posp_soll),3))
cat("eucou_power vs EUCOU (ref):    "); print(round(ac(m$eucou_power,m$EUCOU),3))
# Standardpositionen zum Quervergleich: passt vielleicht mean/median exakt?
cat("eucou_mean  vs EUCOU (ref):    "); print(round(ac(m$eucou_mean,m$EUCOU),3))
cat("eucou_median vs EUCOU (ref):   "); print(round(ac(m$eucou_median,m$EUCOU),3))
# je Periode: posp-Fehler
m$period <- eu_period(m$g105)
cat("\nposp-Abweichung je Periode (mean |mein-soll|):\n")
m |> group_by(period) |> summarise(d=mean(abs(posp-posp_soll),na.rm=TRUE), n=n(), .groups="drop") |> as.data.frame() |> print(row.names=FALSE)
cat("\nHypothesen fuer posp_soll (Perioden 1-4):\n")
m14 <- m |> filter(period<=4)
cat("  eucou_pres:            "); print(round(ac(m14$eucou_pres, m14$posp_soll),3))
cat("  eucou_max:             "); print(round(ac(m14$eucou_max, m14$posp_soll),3)) # falls vorhanden
cat("  (eucou_pres+eucou_mean)/2:"); print(round(ac((m14$eucou_pres+m14$eucou_mean)/2, m14$posp_soll),3))
cat("\nBeispiel Perioden 3-4:\n")
print(m|>filter(g105>=150,g105<=158)|>select(g105,eucou_pres,eucou_mean,pos4,posp_soll)|>mutate(across(where(is.numeric),~round(.,2)))|>as.data.frame(),row.names=FALSE)
