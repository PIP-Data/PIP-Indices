# EIN-KOMMANDO-REFRESH der App-Daten nach einem Daten- ODER Engine-Update.
# Reihenfolge ist wichtig: 25 (EU-Defaults) MUSS vor 23 laufen, da die Standardmodelle seit
# VP_RANGE_EU die EU-Position je Quartal brauchen (23 bricht sonst mit klarer Fehlermeldung ab).
# Aufruf:  Rscript update_app.R
eng <- "K:/DATEIEN/Data/PIP2022/ASPM_WebApp/engine"
rscript <- file.path(R.home("bin"), "Rscript.exe")
run <- function(f){
  cat("\n==== ", f, " ====\n", sep="")
  st <- system2(rscript, shQuote(file.path(eng, f)), stdout=TRUE, stderr=TRUE)
  cat(tail(st, 8), sep="\n"); cat("\n")
}
run("20_build_appdata.R")        # Rohdaten -> schlanke .rds
run("22_build_apppkg.R")         # engine.R (Kopie lib_aspm.R) + nativer Regressionstest
run("25_precompute_eu.R")        # EU-Institutionen (Default) -> eu_defaults.csv (VOR 23!)
run("23_precompute_defaults.R")  # Standardmodelle (config-getrieben, inkl. VP_RANGE_EU) -> defaults.csv
run("24_precompute_parties.R")   # Parteipositionen -> parties.csv
run("26_build_eu_appdata.R")     # EU-Custom-Daten -> eu_data.rds + external.rds + govpos_eu.rds
run("27_build_eu_pkg.R")         # engine_eu.R (Kopie lib_aspm_eu.R) + nativer EU-Custom-Test
run("28_build_customindex.R")    # Custom-Index-Daten (per*f) -> custom_index.rds + per_codes.txt
cat("\nFERTIG. App-Daten aktualisiert (Country + Parties + EU + Custom-Index).\n")
cat("Zum Testen:  Rscript serve.R  ->  http://localhost:8765\n")
