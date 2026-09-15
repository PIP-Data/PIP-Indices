# Custom-Index als 4. Dimension (Country/EU): braucht die per*f-Spalten fuer ALLE Zeilen von d
# (pip_ts_aspm.rds, 212828 Zeilen), nicht nur die ~51K mit vorhandenen per-Daten (das war die
# Einschraenkung von custom_index.rds/28_build_customindex.R fuer den reinen Partei-Reiter).
# Export ueber den stabilen "id"-Schluessel, damit sich per*-Spalten in WebR per Join an d UND
# eu_data anhaengen lassen (beide haben "id").
libpath <- "C:/Users/User/Documents/R/win-library/4.6"; .libPaths(c(libpath, .libPaths()))
suppressMessages({library(haven)})
app <- "K:/DATEIEN/Data/PIP2022/ASPM_WebApp/app"
fw  <- "K:/DATEIEN/Data/PIP2022/_PIP_COLLECTION_v2022-03/03 pip_analysis/ASPM_Extended_Replication_2022/Framework"

d <- as.data.frame(read_dta(file.path(fw,"datasets/pip_ts_aspm.dta")))
perf <- grep("^per.*f$", names(d), value=TRUE)
cp <- d[, c("id", perf)]
names(cp) <- c("id", sub("f$","",perf))                  # per101f -> per101 (gleiche Konvention wie custom_index.rds)
cp[-1] <- lapply(cp[-1], function(x) round(x,3))
saveRDS(cp, file.path(app,"data","custom_per.rds"), compress="xz")

sz <- file.info(file.path(app,"data","custom_per.rds"))$size/1024/1024
cat(sprintf("custom_per.rds: %d Zeilen, %d per-Spalten, %.2f MB\n", nrow(cp), length(perf), sz))
cat("id eindeutig?", !any(duplicated(cp$id)), " Beispiel-Codes:", paste(head(names(cp)[-1],4),collapse=","), "...\n")
