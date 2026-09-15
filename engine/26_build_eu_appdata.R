# EU-Custom (WebR): schlanke Datendateien fuer estimate_eu im Browser.
#  eu_data.rds  = EU-Mitglieder-Zeilen, nur fuer die EU-Institutionen noetige Spalten
#  external.rds = population/gdp_cur je (g101,g103)
#  govpos_eu.rds= vorberechnete Laender-GOV_POS (Default-Specs) je Dimension (EU rechnet darauf)
libpath <- "C:/Users/User/Documents/R/win-library/4.6"; .libPaths(c(libpath, .libPaths()))
suppressMessages({library(haven); library(dplyr)})
eng <- "K:/DATEIEN/Data/PIP2022/ASPM_WebApp/engine"
app <- "K:/DATEIEN/Data/PIP2022/ASPM_WebApp/app"
fw  <- "K:/DATEIEN/Data/PIP2022/_PIP_COLLECTION_v2022-03/03 pip_analysis/ASPM_Extended_Replication_2022/Framework"
source(file.path(eng,"lib_aspm_eu.R"))

d   <- as.data.frame(read_dta(file.path(fw,"datasets/pip_ts_aspm.dta")))
ext <- as.data.frame(read_dta(file.path(fw,"datasets/external_data.dta")))
ref <- as.data.frame(read_dta(file.path(fw,"datasets/swi_referenda.dta")))

# 1) EU-Mitglieder (ever) + benoetigte Spalten
keep <- c("id","g101","g102","g103","g104","g105","g106","p101","ja10f","ja20f","bu01f",
          "p502","p503","p123","p601","p604","p611","p612","p614","p615","p625",
          paste0("p", 201:218),    # nationale Ressorts (Ministerrat: Politikfeld)
          paste0("p", 631:648))    # Kommissar-Ressorts (Kommission: Politikfeld)
keep <- intersect(keep, names(d))
dd <- d |> group_by(g101) |> mutate(.euk=suppressWarnings(mean(p601,na.rm=TRUE))) |> ungroup()
dd <- dd[!is.na(dd$.euk)&!is.nan(dd$.euk), keep]
# String-Spalten (Namen) als character, sonst schlank
saveRDS(dd, file.path(app,"data","eu_data.rds"), compress="xz")

# 2) externe Daten schlank
extk <- ext |> select(g101, g103, population, gdp_cur)
saveRDS(extk, file.path(app,"data","external.rds"), compress="xz")

# 3) GOV_POS je Dimension (Default-Specs)
cty <- read.csv(file.path(app,"config","countries.csv"), colClasses="character", check.names=FALSE)
dcsv<- read.csv(file.path(app,"config","dimensions.csv"), colClasses="character", check.names=FALSE)
specs <- setNames(as.list(cty$default_spec), cty$num)
gp <- list()
for(i in seq_len(nrow(dcsv))){ di<-dcsv$column[i]; key<-dcsv$key[i]
  q <- estimate_aspm(d, specs, ideo=di, referenda=ref, verbose=FALSE)$quarterly
  gp[[key]] <- q |> transmute(iso, techq, GOV_POS, dim=di) }
govpos <- bind_rows(gp)
saveRDS(govpos, file.path(app,"data","govpos_eu.rds"), compress="xz")

for(f in c("eu_data.rds","external.rds","govpos_eu.rds")){
  cat(sprintf("  %-16s %.2f MB\n", f, file.info(file.path(app,"data",f))$size/1024/1024)) }
cat("eu_data:", nrow(dd), "Zeilen,", length(keep), "Spalten\n")
