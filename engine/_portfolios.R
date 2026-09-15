libpath <- "C:/Users/User/Documents/R/win-library/4.6"; .libPaths(c(libpath, .libPaths()))
suppressMessages(library(haven))
fw <- "K:/DATEIEN/Data/PIP2022/_PIP_COLLECTION_v2022-03/03 pip_analysis/ASPM_Extended_Replication_2022/Framework"
d <- read_dta(file.path(fw,"datasets/pip_ts_aspm.dta"), n_max=1)
p2 <- grep("^p2[0-9]+$", names(d), value=TRUE)
cat("Anzahl p2xx-Spalten:", length(p2), "\n\n")
for(v in p2){
  l <- attr(d[[v]],"label"); if(is.null(l)) l <- ""
  cat(sprintf("%-8s %s\n", v, l))
}
