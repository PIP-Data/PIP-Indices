# Inspektion: pip_ts_aspm.dta (Engine-Input) + aspm_lr_quarterly.dta (Referenz-Output).
libpath <- "C:/Users/User/Documents/R/win-library/4.6"; .libPaths(c(libpath, .libPaths()))
suppressMessages({library(haven); library(dplyr)})
fw <- "K:/DATEIEN/Data/PIP2022/ASPM Extended Replication/Release/Framework"
d <- read_dta(file.path(fw, "datasets/pip_ts_aspm.dta"))
cat("== pip_ts_aspm:", nrow(d), "x", ncol(d), "==\n")
key <- c("id","g101","g102","g103","g104","g105","g106","p101","p103","p118","p303","p402","p502","p601","ja10f","ja20f")
cat("Schluessel vorhanden:", paste(key[key %in% names(d)], collapse=", "), "\n")
cat("FEHLT:", paste(key[!key %in% names(d)], collapse=", "), "\n")
cat("g106-Werte:", paste(sort(unique(d$g106)), collapse=","), "\n")
cat("p103-Werte:", paste(sort(unique(d$p103)), collapse=","), "\n")
# Austria (40) ein Quartal
a <- d |> filter(g101==40) |> filter(g105==max(g105[!is.na(p303)]))
cat("\n== Austria, letztes Quartal (g105=",a$g105[1],") ==\n",sep="")
print(a |> select(p101,p103,p118,p303,p402,p502,ja10f) |> as.data.frame(), row.names=FALSE)

# Referenz-Output
r <- read_dta(file.path(fw, "aspm_lr_quarterly.dta"))
cat("\n== aspm_lr_quarterly:", nrow(r), "x", ncol(r), "==\n")
cat("Spalten:", paste(names(r), collapse=", "), "\n")
cat("\nAustria letzte 3 Zeilen:\n")
print(r |> filter(iso==40) |> tail(3) |> select(any_of(c("iso","year","quarter","median1st","median2nd","pres","GOV_POS","govmin","govmax","minoritygov","VP_RANGE"))) |> as.data.frame(), row.names=FALSE)
