# Politikfeld-Erweiterung: (a) Regression gegen die bisherigen Defaults, (b) Wirkung der neuen.
libpath <- "C:/Users/User/Documents/R/win-library/4.6"; .libPaths(c(libpath, .libPaths()))
suppressMessages({library(haven); library(dplyr)})
eng <- "K:/DATEIEN/Data/PIP2022/ASPM_WebApp/engine"
app <- "K:/DATEIEN/Data/PIP2022/ASPM_WebApp/app"
fw  <- "K:/DATEIEN/Data/PIP2022/_PIP_COLLECTION_v2022-03/03 pip_analysis/ASPM_Extended_Replication_2022/Framework"
source(file.path(eng,"lib_aspm_eu.R"))

d   <- as.data.frame(read_dta(file.path(fw,"datasets/pip_ts_aspm.dta")))
ext <- as.data.frame(read_dta(file.path(fw,"datasets/external_data.dta")))
ref <- as.data.frame(read_dta(file.path(fw,"datasets/swi_referenda.dta")))
cty <- read.csv(file.path(app,"config","countries.csv"), colClasses="character", check.names=FALSE)
specs <- setNames(as.list(cty$default_spec), cty$num)

old <- read.csv(file.path(app,"data","eu_defaults.csv"))
cmp <- function(a,b,lab){
  names(a) <- c("techq","v1"); names(b) <- c("techq","v2")
  m <- merge(a,b,by="techq"); v1<-m$v1; v2<-m$v2; k<-!is.na(v1)&!is.na(v2)
  cat(sprintf("  %-14s n=%3d cor=%.6f exakt=%.3f maxdiff=%.4f\n", lab, sum(k),
      stats::cor(v1[k],v2[k]), mean(abs(v1[k]-v2[k])<1e-4), max(abs(v1[k]-v2[k]))))
}

for(dm in c("LR","GG","RILE")){
  di <- c(LR="ja10f",GG="ja20f",RILE="bu01f")[[dm]]
  cat("\n===",dm,"(",di,")===\n")
  gov <- estimate_aspm(d, specs, ideo=di, referenda=ref, verbose=FALSE)$quarterly
  o   <- old[old$dim==dm,]

  # (a) ALTE Einstellung: Kommission Umwelt-Kaskade, Ministerrat ohne Ressort
  a <- estimate_eu(d, di, gov, external=ext,
        spec=list(comm_portf=c("p645","p643"), coun_portf=NULL))
  cat(" Regression (alte Einstellung vs. eu_defaults.csv):\n")
  cmp(a[,c("g105","COMM_POS")],    o[,c("techq","COMM_POS")],    "COMM_POS")
  cmp(a[,c("g105","COUNCIL_POS")], o[,c("techq","COUNCIL_POS")], "COUNCIL_POS")
  cmp(a[,c("g105","EU_POS")],      o[,c("techq","EU_POS")],      "EU_POS")

  # (b) NEUE Defaults: Wirtschaft, bei GG Umwelt
  pf <- if(dm=="GG") c("p215","p213") else c("p208","p207","p214")
  b <- estimate_eu(d, di, gov, external=ext,
        spec=list(comm_portf=eu_comm_portf(pf), coun_portf=pf))
  cat(sprintf(" Neuer Default (%s -> %s) vs. bisher:\n",
      paste(pf,collapse=","), paste(eu_comm_portf(pf),collapse=",")))
  cmp(b[,c("g105","COMM_POS")],    o[,c("techq","COMM_POS")],    "COMM_POS")
  cmp(b[,c("g105","COUNCIL_POS")], o[,c("techq","COUNCIL_POS")], "COUNCIL_POS")
  cmp(b[,c("g105","EU_POS")],      o[,c("techq","EU_POS")],      "EU_POS")
  cat(" Beispielwerte neu (letzte 3 Quartale):\n")
  print(tail(b[!is.na(b$EU_POS),c("g105","COMM_POS","COUNCIL_POS","EU_POS")],3), row.names=FALSE)
}
