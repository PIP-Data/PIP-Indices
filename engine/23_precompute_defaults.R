# HYBRID-ARCHITEKTUR, Teil 1: Standardmodelle OFFLINE vorberechnen.
# Rechnet DEFAULT_SPECS x 36 Laender x 3 Dimensionen (LR/GG/RILE) und legt sie als schlanke
# app/data/defaults.csv ab -> App zeigt den Standardfall INSTANT ohne WebR.
libpath <- "C:/Users/User/Documents/R/win-library/4.6"; .libPaths(c(libpath, .libPaths()))
suppressMessages({library(dplyr)})
app <- "K:/DATEIEN/Data/PIP2022/ASPM_WebApp/app"
source(file.path(app,"engine.R")); source(file.path(app,"wrapper.R"))
d <<- readRDS(file.path(app,"data","pip_ts_aspm.rds"))
referenda <<- readRDS(file.path(app,"data","swi_referenda.rds"))

# EU-Positionen je Dimension (fuer VP_RANGE_EU) - braucht eu_defaults.csv, also MUSS
# 25_precompute_eu.R vor diesem Skript gelaufen sein (s. update_app.R Reihenfolge).
eud_path <- file.path(app,"data","eu_defaults.csv")
if(!file.exists(eud_path)) stop("eu_defaults.csv fehlt - zuerst 25_precompute_eu.R ausfuehren.")
eud <- utils::read.csv(eud_path, stringsAsFactors=FALSE)
eu_pos_by_dim <- lapply(split(eud, eud$dim), function(x) data.frame(techq=x$techq, pos=x$EU_POS))

# Config aus denselben CSVs, die auch die App nutzt -> EINE Quelle der Wahrheit.
cfg <- file.path(app,"config")
cty <- read.csv(file.path(cfg,"countries.csv"), colClasses="character", check.names=FALSE)
dcsv <- read.csv(file.path(cfg,"dimensions.csv"), colClasses="character", check.names=FALSE)
DIMS <- setNames(dcsv$column, dcsv$key)          # z.B. c(LR="ja10f", GG="ja20f", RILE="bu01f")
DIMPF <- setNames(dcsv$default_portf, dcsv$key)  # z.B. c(LR="p208,p207,p214", GG="p215,p213", ...)
isos  <- cty$num
cat("Config:", nrow(cty), "Laender,", nrow(dcsv), "Dimensionen (", paste(names(DIMS),collapse="/"), ")\n")
all <- list()
for(nm in names(DIMS)){
  di <- DIMS[[nm]]
  # Platzhalter "DIMPF" in countries.csv (gov(minister:DIMPF)/gov(pmnegot:DIMPF)) je Dimension aufloesen.
  # Country-Tab-Oberflaeche zeigt nur 2 Ressort-Faelle (1st/2nd) -> erste 2 der Vorgabe nehmen,
  # exakt wie app/index.html es beim Rendern der Standardmodelle tut (muss deckungsgleich bleiben).
  pf2 <- paste(head(strsplit(DIMPF[[nm]], ",")[[1]], 2), collapse=",")
  specs <- gsub("DIMPF", pf2, cty$default_spec, fixed=TRUE)
  t0 <- Sys.time()
  # Ein default_spec kann mehrere Perioden enthalten ("... || 2001: ..."), s. app/index.html parseSegs().
  # Einsegment-Laender laufen wie bisher in EINEM Aufruf; nur die wenigen Mehrperioden-Laender werden
  # Segment fuer Segment gerechnet und danach am Jahr zusammengeschnitten - genau wie im Browser.
  segl  <- strsplit(specs, "||", fixed=TRUE)
  einf  <- which(lengths(segl) == 1)
  mehrf <- which(lengths(segl) >  1)
  r <- aspm_query(isos[einf], trimws(sapply(segl[einf], `[[`, 1)), ideo=di, eu_pos=eu_pos_by_dim[[nm]])
  for(i in mehrf){
    sg <- trimws(segl[[i]])
    startjahr <- function(s) if(grepl("^\\d{4}\\s*:", s)) as.numeric(sub("^(\\d{4}).*$","\\1",s)) else -Inf
    for(k in seq_along(sg)){
      von <- if(k==1) -Inf else startjahr(sg[k])
      bis <- if(k==length(sg)) Inf else startjahr(sg[k+1]) - 1
      rk <- aspm_query(isos[i], sub("^\\d{4}\\s*:\\s*","",sg[k]), ideo=di, eu_pos=eu_pos_by_dim[[nm]])
      r  <- rbind(r, rk[rk$year >= von & rk$year <= bis, ])
    }
    cat(sprintf("     %s: %d Perioden\n", cty$iso3[i], length(sg)))
  }
  r <- r[order(as.numeric(r$iso), as.numeric(r$techq)), ]
  r <- r[!is.na(r$GOV_POS), ]                     # Leerquartale raus
  r$country <- NULL; r$dim <- nm
  r <- r[, c("dim","iso","year","quarter","techq","eu","median1st","median2nd","pres",
             "GOV_POS","govmin","govmax","VP_RANGE","VP_RANGE_EU","minoritygov")]
  all[[nm]] <- r
  cat(sprintf("  %-4s (%s): %d Zeilen, %.1f s\n", nm, di, nrow(r),
              as.numeric(difftime(Sys.time(),t0,units="secs"))))
}
def <- bind_rows(all)
utils::write.csv(def, file.path(app,"data","defaults.csv"), row.names=FALSE, na="")
sz <- file.info(file.path(app,"data","defaults.csv"))$size/1024/1024
cat(sprintf("\ndefaults.csv: %d Zeilen, %.2f MB (unkomprimiert; hosting-gzip ~x5 kleiner)\n", nrow(def), sz))
cat("Dimensionen:", paste(names(DIMS),collapse="/"), " Laender:", length(isos), "\n")
