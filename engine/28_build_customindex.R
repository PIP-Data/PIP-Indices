# Custom-Index (WebR): Manifesto-Flow-Kategorien je Partei/Quartal, damit der Nutzer eigene
# Ideologie-Formeln (z.B. (per101+per407)/2) rechnen kann. Spalten per***f -> per*** (Nutzer tippt per101).
libpath <- "C:/Users/User/Documents/R/win-library/4.6"; .libPaths(c(libpath, .libPaths()))
suppressMessages({library(haven); library(dplyr)})
app <- "K:/DATEIEN/Data/PIP2022/ASPM_WebApp/app"
fw  <- "K:/DATEIEN/Data/PIP2022/_PIP_COLLECTION_v2022-03/03 pip_analysis/ASPM_Extended_Replication_2022/Framework"
d <- as.data.frame(read_dta(file.path(fw,"datasets/pip_ts_aspm.dta")))
perf <- grep("^per.*f$", names(d), value=TRUE)
has  <- rowSums(!is.na(d[,perf])) > 0
ci <- d[has, c("g101","g103","g104","g105","p101","cmp09_partyname", perf)]
names(ci) <- c("iso","year","quarter","techq","party","name", sub("f$","",perf))   # per101f -> per101
# Namens-Lookup (nur ~2% Zeilen benannt) auf ganze Partei uebernehmen. Neuester Name (nicht first()!),
# da MARPOR-Partycodes bei Fusionen/Umbenennungen erhalten bleiben (z.B. PDS -> Die Linke 2007).
nm <- ci |> filter(!is.na(name)&trimws(name)!="") |> arrange(techq) |> group_by(party) |> summarise(nm=last(name),.groups="drop")
ci <- ci |> left_join(nm, by="party") |> mutate(name=ifelse(is.na(nm),"",nm)) |> select(-nm)
percols <- sub("f$","",perf)
ci[percols] <- lapply(ci[percols], function(x) round(x,3))
saveRDS(ci, file.path(app,"data","custom_index.rds"), compress="xz")
# Liste der verfuegbaren per-Codes fuer die UI (als kleine JSON-freundliche Datei)
writeLines(paste(percols, collapse=","), file.path(app,"data","per_codes.txt"))
cat(sprintf("custom_index.rds: %d Zeilen, %d per-Spalten, %.2f MB\n",
    nrow(ci), length(percols), file.info(file.path(app,"data","custom_index.rds"))$size/1024/1024))
cat("per-Codes:", length(percols), "(", percols[1], "...", percols[length(percols)], ")\n")
