# PARTEIEN-Reiter: Positionen der einzelnen Parteien ueber Zeit (direkter Datenauszug, keine ASPM-
# Aggregation). Erzeugt app/data/parties.csv (long) + Namens-Lookup (p101 -> Name, an ~2% der Zeilen
# gefuellt -> pro Partei uebernommen). Wird lazy geladen, wenn der Parteien-Reiter zuerst geoeffnet wird.
libpath <- "C:/Users/User/Documents/R/win-library/4.6"; .libPaths(c(libpath, .libPaths()))
suppressMessages({library(haven); library(dplyr)})
app <- "K:/DATEIEN/Data/PIP2022/ASPM_WebApp/app"
fw  <- "K:/DATEIEN/Data/PIP2022/_PIP_COLLECTION_v2022-03/03 pip_analysis/ASPM_Extended_Replication_2022/Framework"
# Direkt aus der Rohdatei: die vollen Index-Familien (ja11-15/ja21-25) stehen bewusst NICHT in der
# schlanken App-.rds, damit die von WebR geladen wird klein bleibt (s. 20_build_appdata.R).
d <- as.data.frame(read_dta(file.path(fw,"datasets/pip_ts_aspm.dta")))

# Namens-Lookup: ein Name je Partei. Neuester bekannter Name (nicht first()!), da MARPOR-Partycodes
# bei Fusionen/Umbenennungen erhalten bleiben (z.B. p101=41221: PDS -> Die Linke 2007, gleicher Code).
nm <- d |> filter(!is.na(cmp09_partyname) & trimws(cmp09_partyname)!="") |>
  arrange(g105) |> group_by(p101) |> summarise(name=last(cmp09_partyname), .groups="drop")

# Volle standardisierte Index-Familie je Dimension. COH heisst jetzt eindeutig LR_COH bzw. GG_COH -
# vorher hiess nur die LR-Kohaesion "COH", was nicht erkennen liess auf welche Dimension sie sich bezieht.
VALCOLS <- c("LR","LR_IMP","LR_CORE","LR_CORE_IMP","LR_PLUS","LR_PLUS_IMP","LR_COH",
             "GG","GG_IMP","GG_CORE","GG_CORE_IMP","GG_PLUS","GG_PLUS_IMP","GG_COH","RILE")
p <- d |>
  select(iso=g101, year=g103, quarter=g104, techq=g105, party=p101,
         LR=ja10f, LR_IMP=ja11f, LR_CORE=ja12f, LR_CORE_IMP=ja13f,
         LR_PLUS=ja14f, LR_PLUS_IMP=ja15f, LR_COH=jo01f,
         GG=ja20f, GG_IMP=ja21f, GG_CORE=ja22f, GG_CORE_IMP=ja23f,
         GG_PLUS=ja24f, GG_PLUS_IMP=ja25f, GG_COH=jo02f,
         RILE=bu01f) |>
  filter(!(is.na(LR) & is.na(GG) & is.na(RILE))) |>
  left_join(nm, by=c("party"="p101")) |>
  mutate(name=ifelse(is.na(name),"",name),
         across(all_of(VALCOLS), ~round(.x,4))) |>
  select(iso,year,quarter,techq,party,name, all_of(VALCOLS)) |>
  arrange(iso,party,techq)

utils::write.csv(p, file.path(app,"data","parties.csv"), row.names=FALSE, na="")
sz <- file.info(file.path(app,"data","parties.csv"))$size/1024/1024
cat(sprintf("parties.csv: %d Zeilen, %.2f MB (unkompr.; hosting-gzip ~x5 kleiner)\n", nrow(p), sz))
cat("Parteien gesamt:", n_distinct(p$party), " Laender:", n_distinct(p$iso), " benannt:", sum(nm$name!=""), "\n")
cat("Wertspalten:", paste(VALCOLS, collapse=", "), "\n")
cat("Fuellgrad je Spalte:\n"); print(round(100*colMeans(!is.na(p[VALCOLS])),1))
cat("Beispiel GER:\n"); print(head(p[p$iso==276 & p$name!="", c("party","name","year","LR","LR_IMP","LR_CORE","GG","GG_COH")], 5), row.names=FALSE)
