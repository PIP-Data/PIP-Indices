libpath <- "C:/Users/User/Documents/R/win-library/4.6"; .libPaths(c(libpath, .libPaths()))
suppressMessages({library(dplyr)})
app <- "K:/DATEIEN/Data/PIP2022/ASPM_WebApp/app"
source("K:/DATEIEN/Data/PIP2022/ASPM_WebApp/engine/lib_aspm.R")
d <- readRDS(file.path(app,"data","pip_ts_aspm.rds"))
dc <- d[d$g101==276,]
tt <- function(lbl,expr){t<-system.time(x<-force(expr))["elapsed"];cat(sprintf("  %-22s %.3f s\n",lbl,t));invisible(x)}
cat("Deutschland (minister) Komponenten:\n")
basics <- tt("aspm_basics", aspm_basics(dc,"ja10f"))
gov <- tt("gov_minister", gov_minister(dc,"ja10f",basics,c("p215","p213")))
gov2 <- gov |> left_join(basics|>select(g101,g105,median1st),by=c("g101","g105"))
mo <- tt("minority_mcwc", minority_mcwc(dc,"ja10f",gov2,antisys=character(0)))
momin <- mo|>filter(!is.na(minoritygov)&minoritygov==1)
mmap <- setNames(momin$minogov, paste(momin$g101,momin$g105))
tt("vetoplayer", vetoplayer(dc,"ja10f",basics,gov2,c("gov","2ch"),mmap))
cat("\nGesamt estimate_country:\n")
tt("estimate_country", estimate_country(dc,"ja10f",list(gov="minister",portf=c("p215","p213"),minority=TRUE,antisys=NULL,vp=c("gov","2ch"))))
