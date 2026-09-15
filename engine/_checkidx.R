libpath <- "C:/Users/User/Documents/R/win-library/4.6"; .libPaths(c(libpath, .libPaths()))
suppressMessages(library(haven))
fw <- "K:/DATEIEN/Data/PIP2022/_PIP_COLLECTION_v2022-03/03 pip_analysis/ASPM_Extended_Replication_2022/Framework"
app <- "K:/DATEIEN/Data/PIP2022/ASPM_WebApp/app"
d <- read_dta(file.path(fw,"datasets/pip_ts_aspm.dta"), n_max=1)
labs <- sapply(names(d), function(v){ l<-attr(d[[v]],"label"); if(is.null(l)) "" else l })
idx <- grep("^(ja|jo|bu)[0-9]", names(d), value=TRUE)
cat("=== Alle ja/jo/bu-Indexspalten in der ROHDATEI (", length(idx), ") ===\n", sep="")
for(v in sort(idx)) cat(sprintf("%-8s %s\n", v, labs[[v]]))

rds <- readRDS(file.path(app,"data","pip_ts_aspm.rds"))
cat("\n=== Davon bereits in app/data/pip_ts_aspm.rds ===\n")
cat(paste(sort(intersect(idx, names(rds))), collapse=", "), "\n")
cat("\n=== FEHLEN noch in der App-.rds ===\n")
cat(paste(sort(setdiff(idx, names(rds))), collapse=", "), "\n")

# Wie voll sind die f-Varianten (die quartalsweise interpolierten)?
cat("\n=== Fuellgrad der f-Varianten (Anteil nicht-NA, volle Datei) ===\n")
full <- read_dta(file.path(fw,"datasets/pip_ts_aspm.dta"))
for(v in sort(grep("^(ja|jo)[0-9]+f$", names(full), value=TRUE))){
  cat(sprintf("%-8s %5.1f%%  %s\n", v, 100*mean(!is.na(full[[v]])), labs[[v]]))
}
