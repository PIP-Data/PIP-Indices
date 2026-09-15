libpath <- "C:/Users/User/Documents/R/win-library/4.6"; .libPaths(c(libpath, .libPaths()))
suppressMessages({library(dplyr)})
app <- "K:/DATEIEN/Data/PIP2022/ASPM_WebApp/app"
source(file.path(app,"engine.R"))
source(file.path(app,"wrapper.R"))
source(file.path(app,"custom_wrapper.R"))
source(file.path(app,"engine_eu.R"))
source(file.path(app,"eu_wrapper.R"))

d <<- readRDS(file.path(app,"data","pip_ts_aspm.rds"))
referenda <<- readRDS(file.path(app,"data","swi_referenda.rds"))
custom_per <<- readRDS(file.path(app,"data","custom_per.rds"))
eu_data <<- readRDS(file.path(app,"data","eu_data.rds"))
external_eu <<- readRDS(file.path(app,"data","external.rds"))
govpos_eu <<- readRDS(file.path(app,"data","govpos_eu.rds"))
cat("Alle Datensaetze geladen.\n")

# --- Test 1: Country custom dimension, ein simples Land (GER) ---
formula <- "per501 - per110"     # Umwelt(+) minus Europa(-)
res_custom <- aspm_query_custom(276, "gov(minister:p215,p213) minority(yes;antisys:no) vp(gov,2ch)", formula, gran="quarterly")
cat(sprintf("\nTest 1 - Country Custom (GER, '%s'): %d Zeilen\n", formula, nrow(res_custom)))
cat("NA-Anteil GOV_POS:", round(mean(is.na(res_custom$GOV_POS)),3), "\n")
print(tail(res_custom[,c("iso","year","quarter","GOV_POS","VP_RANGE")],3), row.names=FALSE)

# Gegenprobe: andere Formel muss (meistens) andere Werte liefern
res_custom2 <- aspm_query_custom(276, "gov(minister:p215,p213) minority(yes;antisys:no) vp(gov,2ch)", "per101 + per107", gran="quarterly")
same <- isTRUE(all.equal(res_custom$GOV_POS, res_custom2$GOV_POS))
cat("Verschiedene Formeln liefern verschiedene GOV_POS:", if(!same) "JA (gut)" else "NEIN <-- VERDAECHTIG", "\n")

# Formel-Validierung: ungueltige Formel muss abgelehnt werden
val_ok <- tryCatch({ aspm_query_custom(276, "gov(seats) minority(yes;antisys:no) vp(gov)", "system('ls')"); FALSE },
                    error=function(e) TRUE)
cat("Ungueltige Formel (system(...)) wird abgelehnt:", if(val_ok) "JA" else "NEIN <-- SICHERHEITSPROBLEM", "\n")

# --- Test 2: EU custom dimension ---
cty <- read.csv(file.path(app,"config","countries.csv"), colClasses="character", check.names=FALSE)
csv_out <- eu_run_custom(formula, cty$num, cty$default_spec)
lines <- strsplit(csv_out, "\n")[[1]]
cat(sprintf("\nTest 2 - EU Custom ('%s'): %d Zeilen (inkl. Header)\n", formula, length(lines)))
cat(paste(head(lines,4), collapse="\n"), "\n")

# Vergleich mit Standard-EU-Lauf auf LR (muss existieren und anders sein als Custom, da andere Skala)
csv_lr <- eu_run("ja10f")
cat("\nEU Standard (LR) Zeilen:", length(strsplit(csv_lr,"\n")[[1]])-1, " EU Custom Zeilen:", length(lines)-1, "\n")

cat("\nALLE CUSTOM-DIMENSION-TESTS DURCHGELAUFEN.\n")
