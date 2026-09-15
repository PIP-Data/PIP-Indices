libpath <- "C:/Users/User/Documents/R/win-library/4.6"; .libPaths(c(libpath, .libPaths()))
suppressMessages(library(haven))
fw <- "K:/DATEIEN/Data/PIP2022/_PIP_COLLECTION_v2022-03/03 pip_analysis/ASPM_Extended_Replication_2022/Framework"
d <- read_dta(file.path(fw,"datasets/pip_ts_aspm.dta"), n_max=1)
for(v in c("ja10f","ja10c","ja10o","ja20f","ja20c","ja20o","bu01f",
           "jo01f","jo01c","jo01o","jo02f","jo02c","jo02o")){
  l <- if(v %in% names(d)) attr(d[[v]],"label") else "<Spalte fehlt>"
  cat(sprintf("%-8s %s\n", v, if(is.null(l)) "(kein Label)" else l))
}
cat("\n--- Suche nach 'imp' im Namen oder Label ---\n")
labs <- sapply(names(d), function(v){ l<-attr(d[[v]],"label"); if(is.null(l)) "" else l })
hit <- grepl("imp", names(d), ignore.case=TRUE) | grepl("import", labs, ignore.case=TRUE)
print(data.frame(var=names(d)[hit], label=labs[hit]))
