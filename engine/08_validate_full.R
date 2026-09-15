# Voller ASPM-Lauf (alle 36 Laender, Default-Specs) vs v2022-Referenz
libpath <- "C:/Users/User/Documents/R/win-library/4.6"; .libPaths(c(libpath, .libPaths()))
suppressMessages({library(haven); library(dplyr)})
fw <- "K:/DATEIEN/Data/PIP2022/_PIP_COLLECTION_v2022-03/03 pip_analysis/ASPM_Extended_Replication_2022/Framework"
source("K:/DATEIEN/Data/PIP2022/ASPM_WebApp/engine/lib_aspm.R")
d <- read_dta(file.path(fw,"datasets/pip_ts_aspm.dta")) |> as.data.frame()
ref <- read_dta(file.path(fw,"aspm_lr_quarterly.dta")) |> as.data.frame()
referenda <- read_dta(file.path(fw,"datasets/swi_referenda.dta")) |> as.data.frame()

DEFAULT_SPECS <- list(
  "124"="gov(pmnegot:p215,p213) minority(yes;antisys:no) vp(gov,2ch)",
  "36" ="gov(pmnegot:p215,p213) minority(yes;antisys:no) vp(gov,2ch)",
  "724"="gov(pmnegot:p215,p213) minority(yes;antisys:no) vp(gov,2ch)",
  "826"="gov(pmnegot:p215,p213) minority(yes;antisys:no) vp(gov)",
  "554"="gov(pmnegot:p215,p213) minority(yes;antisys:no) vp(gov)",
  "372"="gov(pmnegot:p215,p213) minority(yes;antisys:53951) vp(gov)",
  "620"="gov(pmnegot:p215,p213) minority(yes;antisys:no) vp(gov,pres)",
  "250"="gov(pmnegot:p215,p213) minority(yes;antisys:31720) vp(special_aspm)",
  "208"="gov(pmnegot:p215,p213) minority(yes;antisys:13951) vp(gov,2ch)",
  "352"="gov(pmnegot:p215,p213) minority(yes;antisys:no) vp(gov)",
  "380"="gov(minister:p215,p213) minority(yes;antisys:32710) vp(gov,2ch)",
  "276"="gov(minister:p215,p213) minority(yes;antisys:no) vp(gov,2ch)",
  "300"="gov(minister:p215,p213) minority(yes;antisys:no) vp(gov)",
  "392"="gov(minister:p215,p213) minority(yes;antisys:no) vp(gov,2ch)",
  "40" ="gov(seats) minority(yes;antisys:no) vp(gov)",
  "246"="gov(seats) minority(yes;antisys:no) vp(gov,pres)",
  "528"="gov(seats) minority(yes;antisys:no) vp(gov,2ch)",
  "578"="gov(seats) minority(yes;antisys:12951) vp(gov)",
  "752"="gov(seats) minority(yes;antisys:11951) vp(gov,2ch)",
  "56" ="gov(seats) minority(yes;antisys:no) vp(gov,2ch)",
  "756"="gov(seats) minority(yes;antisys:no) vp(special_aspm)",
  "840"="gov(special_aspm) minority(yes;antisys:no) vp(special_aspm)",
  "196"="gov(seats) minority(yes;antisys:no) vp(gov)",
  "442"="gov(seats) minority(yes;antisys:no) vp(gov)",
  "470"="gov(seats) minority(yes;antisys:no) vp(gov)",
  "100"="gov(seats) minority(yes;antisys:no) vp(gov,pres)",
  "191"="gov(seats) minority(yes;antisys:no) vp(gov)",
  "203"="gov(seats) minority(yes;antisys:no) vp(gov)",
  "233"="gov(seats) minority(yes;antisys:no) vp(gov)",
  "348"="gov(seats) minority(yes;antisys:no) vp(gov)",
  "428"="gov(seats) minority(yes;antisys:no) vp(gov,pres)",
  "440"="gov(seats) minority(yes;antisys:no) vp(gov,pres)",
  "616"="gov(seats) minority(yes;antisys:no) vp(gov)",
  "642"="gov(seats) minority(yes;antisys:no) vp(gov,2ch)",
  "703"="gov(seats) minority(yes;antisys:no) vp(gov)",
  "705"="gov(seats) minority(yes;antisys:no) vp(gov)"
)
t0 <- Sys.time()
out <- estimate_aspm(d, DEFAULT_SPECS, ideo="ja10f", referenda=referenda, verbose=FALSE)
cat("Laufzeit:", round(as.numeric(difftime(Sys.time(),t0,units="secs")),1), "s\n")
cat("Quartals-Output:", nrow(out$quarterly), "Zeilen,", n_distinct(out$quarterly$iso), "Laender\n")
cat("Jahres-Output:", nrow(out$yearly), "Zeilen\n\n")

cmp <- out$quarterly |> inner_join(ref|>select(iso,techq,median1st,median2nd,pres,GOV_POS,govmin,govmax,VP_RANGE,minoritygov),
                                   by=c("iso","techq"), suffix=c("",".r"))
metric <- function(a,b){ok<-!is.na(a)&!is.na(b);if(!sum(ok))return(c(NA,NA,NA));c(cor(a[ok],b[ok]),max(abs(a[ok]-b[ok])),mean(abs(a[ok]-b[ok])<1e-3))}
cat("== VOLLER LAUF (36 Laender) R vs v2022-Referenz ==  n =", nrow(cmp), "\n")
for(v in c("median1st","median2nd","pres","GOV_POS","govmin","govmax","VP_RANGE","minoritygov")){
  m<-metric(cmp[[v]],cmp[[paste0(v,".r")]]); cat(sprintf("  %-12s cor %.4f  maxdiff %.4g  exakt %.3f\n",v,m[1],m[2],m[3]))
}
saveRDS(out, "K:/DATEIEN/Data/PIP2022/ASPM_WebApp/engine/aspm_lr_out.rds")
