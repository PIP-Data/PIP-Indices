libpath <- "C:/Users/User/Documents/R/win-library/4.6"; .libPaths(c(libpath, .libPaths()))
suppressMessages({library(haven); library(dplyr)})
fw <- "K:/DATEIEN/Data/PIP2022/_PIP_COLLECTION_v2022-03/03 pip_analysis/ASPM_Extended_Replication_2022/Framework"
d <- as.data.frame(read_dta(file.path(fw,"datasets/pip_ts_aspm.dta")))
perf <- grep("^per.*f$", names(d), value=TRUE)   # Flow-Varianten
cat("per*f (flow) Spalten:", length(perf), "\n")
cat("Beispiele:", paste(head(perf,8),collapse=" "), "...\n")
# Zeilen mit mind. einem per*f
hasper <- rowSums(!is.na(d[,perf])) > 0
cat("Zeilen mit per*f-Daten:", sum(hasper), "von", nrow(d), "\n")
# Testweise als CSV-Groesse schaetzen (Keys + party name + alle per*f, gerundet)
sub <- d[hasper, c("g101","g103","g104","g105","p101","cmp09_partyname", perf)]
sub[perf] <- lapply(sub[perf], function(x) round(x,3))
tmp <- tempfile(); utils::write.csv(sub, tmp, row.names=FALSE, na="")
cat(sprintf("Test-CSV (alle per*f): %.1f MB unkomprimiert\n", file.info(tmp)$size/1024/1024))
# Wertebereich (sind es Prozente 0-100?)
cat("per101f Bereich:", paste(round(range(d$per101f,na.rm=TRUE),2),collapse=" .. "), "\n")
# Wie heissen die per-Kategorien? (Variablen-Label)
lab <- sapply(head(perf,5), function(v){ l<-attr(d[[v]],"label"); if(is.null(l)) "" else l })
for(i in seq_along(lab)) cat("  ", names(lab)[i], "=", lab[i], "\n")
