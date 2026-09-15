# engine_eu.R = lib_aspm_eu.R OHNE die absolute source()-Zeile (in WebR wird engine.R separat geladen).
# Danach nativer Test des EU-Custom-Stacks (genau wie WebR).
libpath <- "C:/Users/User/Documents/R/win-library/4.6"; .libPaths(c(libpath, .libPaths()))
suppressMessages({library(dplyr)})
eng <- "K:/DATEIEN/Data/PIP2022/ASPM_WebApp/engine"
app <- "K:/DATEIEN/Data/PIP2022/ASPM_WebApp/app"
# byte-genau kopieren (erhaelt UTF-8; die source()-Zeile ist mit try() gekapselt und stoert WebR nicht)
file.copy(file.path(eng,"lib_aspm_eu.R"), file.path(app,"engine_eu.R"), overwrite=TRUE)

# --- NATIVER TEST (App-Stack) ---
source(file.path(app,"engine.R"))       # lib_aspm (wmedian, estimate_aspm ...)
source(file.path(app,"engine_eu.R"))    # lib_aspm_eu (estimate_eu ...)
source(file.path(app,"eu_wrapper.R"))   # eu_run
eu_data     <<- readRDS(file.path(app,"data","eu_data.rds"))
external_eu <<- readRDS(file.path(app,"data","external.rds"))
govpos_eu   <<- readRDS(file.path(app,"data","govpos_eu.rds"))

t<-Sys.time()
csv <- eu_run("ja10f", council="default", commission="median", councilofmin="default", euparl="default", principal="1")
cat(sprintf("eu_run (Kommission=median): %.1f s\n", as.numeric(difftime(Sys.time(),t,units="secs"))))
cat("CSV-Kopf:\n"); cat(paste(head(strsplit(csv,"\n")[[1]],3),collapse="\n"),"\n")
cat("...letzte Zeile:\n"); cat(tail(strsplit(csv,"\n")[[1]],1),"\n")
