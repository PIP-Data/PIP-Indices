# Baut das App-R-Paket zusammen (engine.R = lib_aspm.R, referenda.rds) und testet den Stack NATIV,
# damit der Teil, den WebR spaeter im Browser ausfuehrt, garantiert korrekt ist.
libpath <- "C:/Users/User/Documents/R/win-library/4.6"; .libPaths(c(libpath, .libPaths()))
suppressMessages({library(haven); library(dplyr)})
eng <- "K:/DATEIEN/Data/PIP2022/ASPM_WebApp/engine"
app <- "K:/DATEIEN/Data/PIP2022/ASPM_WebApp/app"
fw  <- "K:/DATEIEN/Data/PIP2022/_PIP_COLLECTION_v2022-03/03 pip_analysis/ASPM_Extended_Replication_2022/Framework"

# engine.R = Kopie von lib_aspm.R (self-contained, nur dplyr) -> WebR laedt genau diese Datei
file.copy(file.path(eng,"lib_aspm.R"), file.path(app,"engine.R"), overwrite=TRUE)
# swi_referenda als .rds (fuer SWI-Special)
saveRDS(read_dta(file.path(fw,"datasets/swi_referenda.dta")) |> as.data.frame(),
        file.path(app,"data","swi_referenda.rds"), compress="xz")

# --- NATIVER TEST des App-Stacks (genau wie WebR es tun wird) ---
source(file.path(app,"engine.R"))       # Engine
source(file.path(app,"wrapper.R"))      # aspm_query/aspm_csv
d <<- readRDS(file.path(app,"data","pip_ts_aspm.rds"))
referenda <<- readRDS(file.path(app,"data","swi_referenda.rds"))
cat("Daten geladen:", nrow(d), "Zeilen\n")

isos  <- c(276, 40, 826, 756)
specs <- c("gov(minister:p215,p213) minority(yes;antisys:no) vp(gov,2ch)",
           "gov(seats) minority(yes;antisys:no) vp(gov)",
           "gov(pmnegot:p215,p213) minority(yes;antisys:no) vp(gov)",
           "gov(seats) minority(yes;antisys:no) vp(special_aspm)")
t <- system.time(res <- aspm_query(isos, specs, ideo="ja10f"))
cat(sprintf("aspm_query 4 Laender: %.2f s, %d Zeilen, Spalten: %s\n",
    t["elapsed"], nrow(res), paste(names(res),collapse=",")))
cat("\nBeispiel (Deutschland, letzte 3 Quartale):\n")
print(tail(res[res$iso==276, c("iso","year","quarter","GOV_POS","VP_RANGE","VP_RANGE_EU")],3), row.names=FALSE)
cat("\nCSV-Kopf:\n"); cat(substr(aspm_csv(head(res,2)),1,200),"\n")

# --- Test: VP_RANGE_EU wird IMMER mitgefuehrt (keine vp-Option mehr, sondern feste zweite Spalte) ---
eud <- utils::read.csv(file.path(app,"data","eu_defaults.csv"), stringsAsFactors=FALSE)
eup <- data.frame(techq=eud$techq[eud$dim=="LR"], pos=eud$EU_POS[eud$dim=="LR"])
ger <- aspm_query(276, "gov(minister:p215,p213) minority(yes;antisys:no) vp(gov,2ch)", ideo="ja10f", eu_pos=eup)
ger$d <- round(ger$VP_RANGE_EU - ger$VP_RANGE, 4)
cat(sprintf("\nVP_RANGE_EU GER: %d Quartale, davon Mitglied %d; VP_RANGE_EU groesser in %d, kleiner in %d, gleich in %d\n",
  nrow(ger), sum(!is.na(ger$eu)&ger$eu>0), sum(ger$d>0,na.rm=TRUE), sum(ger$d<0,na.rm=TRUE), sum(ger$d==0,na.rm=TRUE)))
nonmem <- ger[is.na(ger$eu)|ger$eu<=0,]
cat(sprintf("Nicht-Mitgliedsquartale: VP_RANGE_EU == VP_RANGE: %s\n",
  if(!nrow(nonmem)||all(nonmem$d==0,na.rm=TRUE)) "JA" else "NEIN <-- FEHLER"))
cat(sprintf("VP_RANGE_EU nie kleiner als VP_RANGE: %s\n", if(all(ger$d>=0,na.rm=TRUE)) "JA" else "NEIN <-- FEHLER"))
# Gegenprobe Nicht-Mitglied: Japan muss VP_RANGE_EU == VP_RANGE in JEDEM Quartal haben
jpn <- aspm_query(392,"gov(minister:p215,p213) minority(yes;antisys:no) vp(gov,2ch)",ideo="ja10f",eu_pos=eup)
cat(sprintf("Japan (nie EU-Mitglied): VP_RANGE_EU == VP_RANGE ueberall: %s\n",
  if(isTRUE(all.equal(jpn$VP_RANGE, jpn$VP_RANGE_EU))) "JA" else "NEIN <-- FEHLER"))
# Frankreich-Spezialmodell: VP_RANGE_EU muss ebenfalls sauber mitgefuehrt werden (Kohabitations-Fallunterscheidung)
fra <- aspm_query(250,"gov(pmnegot:p215,p213) minority(yes;antisys:31720) vp(special_aspm)",ideo="ja10f",eu_pos=eup)
fra$d <- round(fra$VP_RANGE_EU - fra$VP_RANGE, 4)
cat(sprintf("Frankreich (special_aspm): %d Quartale, VP_RANGE_EU nie kleiner: %s, in %d Quartalen groesser\n",
  nrow(fra), if(all(fra$d>=0,na.rm=TRUE)) "JA" else "NEIN <-- FEHLER", sum(fra$d>0,na.rm=TRUE)))
cat("\nApp-Paket bereit in app/: engine.R, wrapper.R, data/pip_ts_aspm.rds, data/swi_referenda.rds\n")
