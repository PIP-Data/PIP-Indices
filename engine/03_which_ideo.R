libpath <- "C:/Users/User/Documents/R/win-library/4.6"; .libPaths(c(libpath, .libPaths()))
suppressMessages({library(haven); library(dplyr)})
fw <- "K:/DATEIEN/Data/PIP2022/ASPM Extended Replication/Release/Framework"
source("K:/DATEIEN/Data/PIP2022/ASPM_WebApp/engine/lib_aspm.R")
d <- read_dta(file.path(fw,"datasets/pip_ts_aspm.dta")) |> as.data.frame()
ref <- read_dta(file.path(fw,"aspm_lr_quarterly.dta")) |> as.data.frame()

cat("Referenz-Positionsbereiche:\n")
for(v in c("median1st","GOV_POS","govmin","govmax")) cat(sprintf("  %-10s %.1f .. %.1f\n",v,min(ref[[v]],na.rm=TRUE),max(ref[[v]],na.rm=TRUE)))
cat("\nja10-Varianten Bereiche:\n")
for(v in c("ja10o","ja10f","ja10c")) if(v%in%names(d)) cat(sprintf("  %-6s %.1f .. %.1f\n",v,min(d[[v]],na.rm=TRUE),max(d[[v]],na.rm=TRUE)))

# median1st je Variante berechnen, gegen Referenz korrelieren (nur seats-Laender-Teilmenge reicht)
sub <- d |> filter(g101 %in% c(40,56,246,528,578,752))
test <- function(v){
  b <- sub |> group_by(g101,g105) |> summarise(m=wmedian(.data[[v]],p303), .groups="drop")
  m <- b |> inner_join(ref|>select(iso,techq,median1st),by=c("g101"="iso","g105"="techq"))
  ok <- !is.na(m$m)&!is.na(m$median1st)
  c(cor=cor(m$m[ok],m$median1st[ok]), maxdiff=max(abs(m$m[ok]-m$median1st[ok])))
}
cat("\nmedian1st R (je Variante) vs Referenz:\n")
for(v in c("ja10o","ja10f","ja10c")) if(v%in%names(d)){r<-test(v);cat(sprintf("  %-6s cor %.4f  maxdiff %.3g\n",v,r[1],r[2]))}
