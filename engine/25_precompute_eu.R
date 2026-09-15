# EU-Reiter (vorberechnet): je Dimension die EU-Zeitreihe (EUCOU/COMM/COUNCIL/EP/EU_POS).
# Braucht die VOLLEN Daten (EU-Vars p6xx) + external_data; laeuft auf Laender-GOV_POS (estimate_aspm).
libpath <- "C:/Users/User/Documents/R/win-library/4.6"; .libPaths(c(libpath, .libPaths()))
suppressMessages({library(haven); library(dplyr)})
eng <- "K:/DATEIEN/Data/PIP2022/ASPM_WebApp/engine"
app <- "K:/DATEIEN/Data/PIP2022/ASPM_WebApp/app"
fw  <- "K:/DATEIEN/Data/PIP2022/_PIP_COLLECTION_v2022-03/03 pip_analysis/ASPM_Extended_Replication_2022/Framework"
source(file.path(eng,"lib_aspm_eu.R"))     # laedt auch lib_aspm.R (estimate_aspm)

d   <- as.data.frame(read_dta(file.path(fw,"datasets/pip_ts_aspm.dta")))
ext <- as.data.frame(read_dta(file.path(fw,"datasets/external_data.dta")))
referenda <- as.data.frame(read_dta(file.path(fw,"datasets/swi_referenda.dta")))

cty  <- read.csv(file.path(app,"config","countries.csv"), colClasses="character", check.names=FALSE)
dcsv <- read.csv(file.path(app,"config","dimensions.csv"), colClasses="character", check.names=FALSE)
specs <- setNames(as.list(cty$default_spec), cty$num)
DIMS  <- setNames(dcsv$column, dcsv$key)

# techq -> Jahr/Quartal
qmap <- d |> distinct(g105, g103, g104) |> group_by(g105) |>
        summarise(year=g103[1], quarter=g104[1], .groups="drop")

all <- list()
PORTF <- setNames(dcsv$default_portf, dcsv$key)   # Politikfeld-Kaskade je Dimension
for(nm in names(DIMS)){
  di <- DIMS[[nm]]; t0 <- Sys.time()
  pf <- trimws(strsplit(PORTF[[nm]], ",")[[1]]); pf <- pf[nzchar(pf)]
  gov <- estimate_aspm(d, specs, ideo=di, referenda=referenda, verbose=FALSE)$quarterly
  eu  <- estimate_eu(d, di, gov, external=ext,
           spec=list(coun_portf=pf, comm_portf=eu_comm_portf(pf),
                     coun_portf_isos=eu_minister_isos(specs)))
  eu  <- eu |> left_join(qmap, by="g105") |> rename(techq=g105) |>
         mutate(dim=nm, across(c(EUCOU_POS,COMM_POS,COUNCIL_POS,EP_POS,EU_POS), ~round(.x,4))) |>
         filter(!is.na(EU_POS)) |>
         select(dim,year,quarter,techq,EUCOU_POS,COMM_POS,COUNCIL_POS,EP_POS,EU_POS)
  all[[nm]] <- eu
  cat(sprintf("  %-4s (%s, Politikfeld %s, Ministerposition in %d/%d Laendern): %d Quartale, %.1f s\n",
      nm, di, paste(pf,collapse=">"), length(eu_minister_isos(specs)), length(specs),
      nrow(eu), as.numeric(difftime(Sys.time(),t0,units="secs"))))
}
out <- bind_rows(all)
utils::write.csv(out, file.path(app,"data","eu_defaults.csv"), row.names=FALSE, na="")
sz <- file.info(file.path(app,"data","eu_defaults.csv"))$size/1024
cat(sprintf("\neu_defaults.csv: %d Zeilen, %.0f KB\n", nrow(out), sz))
cat("Beispiel (LR, letzte 3):\n"); print(tail(out[out$dim=="LR",],3), row.names=FALSE)
