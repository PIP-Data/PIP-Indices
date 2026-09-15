libpath <- "C:/Users/User/Documents/R/win-library/4.6"; .libPaths(c(libpath, .libPaths()))
app <- "K:/DATEIEN/Data/PIP2022/ASPM_WebApp/app"
d <- readRDS(file.path(app,"data","pip_ts_aspm.rds"))
perf <- grep("^per.*f$", names(d), value=TRUE)
cat("pip_ts_aspm.rds: ", nrow(d), "Zeilen,", ncol(d), "Spalten. per*f-Spalten:", length(perf), "\n")
cat("Beispiele:", paste(head(perf,5),collapse=", "), "...\n")
cat("Groesse Datei:", round(file.info(file.path(app,"data","pip_ts_aspm.rds"))$size/1024/1024,2), "MB\n\n")

eu <- readRDS(file.path(app,"data","eu_data.rds"))
perf_eu <- grep("^per.*f$", names(eu), value=TRUE)
cat("eu_data.rds: ", nrow(eu), "Zeilen,", ncol(eu), "Spalten. per*f-Spalten:", length(perf_eu), "\n")
cat("Alle Spalten:", paste(names(eu),collapse=", "), "\n")
cat("Groesse Datei:", round(file.info(file.path(app,"data","eu_data.rds"))$size/1024/1024,2), "MB\n")
