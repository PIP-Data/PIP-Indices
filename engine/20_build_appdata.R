# Schritt A/1: schlanke App-Datendatei (pip_ts_aspm.rds) bauen + Engine darauf pruefen + Laufzeit.
libpath <- "C:/Users/User/Documents/R/win-library/4.6"; .libPaths(c(libpath, .libPaths()))
suppressMessages({library(haven); library(dplyr)})
fw  <- "K:/DATEIEN/Data/PIP2022/_PIP_COLLECTION_v2022-03/03 pip_analysis/ASPM_Extended_Replication_2022/Framework"
app <- "K:/DATEIEN/Data/PIP2022/ASPM_WebApp/app"; dir.create(file.path(app,"data"), recursive=TRUE, showWarnings=FALSE)
source("K:/DATEIEN/Data/PIP2022/ASPM_WebApp/engine/lib_aspm.R")

d <- read_dta(file.path(fw,"datasets/pip_ts_aspm.dta")) |> as.data.frame()
cat("Original:", nrow(d), "x", ncol(d), "\n")
# verfuegbare Ideologie-/Namensspalten
cat("Dimensionen vorhanden:", paste(intersect(c("ja10f","ja20f","bu01f"),names(d)),collapse=", "), "\n")
namecol <- grep("cmp.*name|partyname|p101name|name", names(d), value=TRUE, ignore.case=TRUE)
cat("moegliche Namensspalten:", paste(head(namecol,8),collapse=", "), "\n")

# benoetigte Spalten: Keys + Engine-p-Vars + Dimensionen + per* (Custom) + Ministerressorts p201-218
keys   <- c("id","g101","g102","g103","g104","g105","g106","p101")
engine <- c("p103","p118","p303","p402","p502","p503","p107","p110","p201",
            paste0("p2",sprintf("%02d",1:18)),"p305","p403","p601")
# Nur die Dimensionen, die die ENGINE im Browser braucht (ja10f/ja20f/bu01f als ideo-Spalte).
# Die weiteren Index-Familien (ja11-15/ja21-25 = Importance/Core/Plus) braucht ausschliesslich der
# Parteien-Reiter und die liest 24_precompute_parties.R direkt aus der .dta -> App-.rds bleibt schlank
# (sie wird bei JEDEM Custom-Laendermodell in WebR heruntergeladen).
dims   <- intersect(c("ja10f","ja10c","ja10o","ja20f","ja20c","ja20o","bu01f"), names(d))
pers   <- grep("^per[0-9]{3,4}$", names(d), value=TRUE)
cohimp <- grep("^(jo|imp)", names(d), value=TRUE)
keep <- unique(c(keys, engine, dims, cohimp, pers, namecol))
keep <- keep[keep %in% names(d)]
slim <- d[, keep]
cat("\nSchlank:", nrow(slim), "x", ncol(slim), "(", length(pers), "per-Spalten,", length(dims), "Dimensionen )\n")

# als komprimiertes .rds (fuer WebR)
saveRDS(slim, file.path(app,"data","pip_ts_aspm.rds"), compress="xz")
sz <- file.info(file.path(app,"data","pip_ts_aspm.rds"))$size
cat("pip_ts_aspm.rds:", round(sz/1024/1024,1), "MB\n")

# Engine auf der schlanken Datei pruefen (Deutschland minister) + Laufzeit realistische Abfrage
sp_ger <- list(gov="minister",portf=c("p215","p213"),minority=TRUE,antisys=NULL,vp=c("gov","2ch"))
t <- system.time(e <- estimate_country(slim[slim$g101==276,],"ja10f",sp_ger))
cat(sprintf("\nDeutschland (minister): %.2f s, %d Quartale, GOV_POS-Bereich %.1f..%.1f\n",
    t["elapsed"], nrow(e), min(e$GOV_POS,na.rm=TRUE), max(e$GOV_POS,na.rm=TRUE)))
# realistische App-Abfrage: 5 Laender gemischt
SP5 <- list("276"="gov(minister:p215,p213) minority(yes;antisys:no) vp(gov,2ch)",
            "250"="gov(pmnegot:p215,p213) minority(yes;antisys:31720) vp(special_aspm)",
            "826"="gov(pmnegot:p215,p213) minority(yes;antisys:no) vp(gov)",
            "40" ="gov(seats) minority(yes;antisys:no) vp(gov)",
            "752"="gov(seats) minority(yes;antisys:11951) vp(gov,2ch)")
ref <- read_dta(file.path(fw,"datasets/swi_referenda.dta")) |> as.data.frame()
t5 <- system.time(out <- estimate_aspm(slim, SP5, ideo="ja10f", referenda=ref, verbose=FALSE))
cat(sprintf("5-Laender-Abfrage: %.2f s (nativ) -> WebR grob x3-8\n", t5["elapsed"]))
cat("DONE\n")
