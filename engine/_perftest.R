libpath <- "C:/Users/User/Documents/R/win-library/4.6"; .libPaths(c(libpath, .libPaths()))
suppressMessages({library(dplyr)})
app <- "K:/DATEIEN/Data/PIP2022/ASPM_WebApp/app"
source(file.path(app,"engine.R")); source(file.path(app,"wrapper.R"))
d <<- readRDS(file.path(app,"data","pip_ts_aspm.rds"))
referenda <<- readRDS(file.path(app,"data","swi_referenda.rds"))
cty <- read.csv(file.path(app,"config","countries.csv"), colClasses="character", check.names=FALSE)

run_n <- function(n){
  sub <- cty[seq_len(min(n,nrow(cty))), ]
  t <- system.time(res <- aspm_query(sub$num, sub$default_spec, ideo="ja10f"))
  cat(sprintf("%2d Laender: %6.2f s  (%d Zeilen)\n", nrow(sub), t["elapsed"], nrow(res)))
}
cat("=== Country-Modell live rechnen (aspm_query), Standard-Specs, EINMALIGER Lauf ===\n")
for(n in c(1,3,10,20,36)) run_n(n)

cat("\n=== Wiederholte Laeufe mit denselben 3 Laendern (wird es langsamer?) ===\n")
for(i in 1:5) run_n(3)

cat("\n=== Minister-Wechsel: 1 Land, verschiedene Ressort-Kombis nacheinander ===\n")
combos <- c("gov(minister:p207,p208)","gov(minister:p203,p204)","gov(minister:p215,p213)",
            "gov(minister:p209,p210)","gov(minister:p211,p212)")
for(sp in combos){
  full <- paste0(sp," minority(yes;antisys:no) vp(gov,2ch)")
  t <- system.time(res <- aspm_query(276, full, ideo="ja10f"))
  cat(sprintf("%-30s %6.2f s\n", sp, t["elapsed"]))
}
