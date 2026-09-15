libpath <- "C:/Users/User/Documents/R/win-library/4.6"; .libPaths(c(libpath, .libPaths()))
app <- "K:/DATEIEN/Data/PIP2022/ASPM_WebApp/app"
d <- readRDS(file.path(app,"data","pip_ts_aspm.rds"))
cat("pip_ts_aspm.rds Spalten:\n"); print(names(d))
cat("\nHat 'id'?", "id" %in% names(d), "\n")
cat("g101,g105,p101 eindeutig?", !any(duplicated(d[,c("g101","g105","p101")])), "\n")

fw  <- "K:/DATEIEN/Data/PIP2022/_PIP_COLLECTION_v2022-03/03 pip_analysis/ASPM_Extended_Replication_2022/Framework"
raw <- as.data.frame(haven::read_dta(file.path(fw,"datasets/pip_ts_aspm.dta"), n_max=5))
cat("\nRohdatei (pip_ts_aspm.dta) hat 'id'?", "id" %in% names(raw), "\n")
cat("Rohdatei Spaltenzahl:", ncol(as.data.frame(haven::read_dta(file.path(fw,"datasets/pip_ts_aspm.dta"), n_max=1))), "\n")
