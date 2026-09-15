libpath <- "C:/Users/User/Documents/R/win-library/4.6"; .libPaths(c(libpath, .libPaths()))
suppressMessages({library(haven); library(dplyr)})
fw <- "K:/DATEIEN/Data/PIP2022/_PIP_COLLECTION_v2022-03/03 pip_analysis/ASPM_Extended_Replication_2022/Framework"
source("K:/DATEIEN/Data/PIP2022/ASPM_WebApp/engine/lib_aspm.R")
d <- read_dta(file.path(fw,"datasets/pip_ts_aspm.dta")) |> as.data.frame()
ref <- read_dta(file.path(fw,"aspm_lr_quarterly.dta")) |> as.data.frame()
cat("ref USA-Spalten:", paste(intersect(c("house","senate","senat","congress","aspi_usa","pres","GOV_POS","vp_usa_int"),names(ref)),collapse=", "),"\n")
dc <- d |> filter(g101==840)
basics <- aspm_basics(dc,"ja10f")
usa <- gov_usa_special(dc,"ja10f",basics) |> left_join(basics|>select(g101,g105,pres),by=c("g101","g105"))
u <- usa |> inner_join(ref|>filter(iso==840)|>select(techq,GOV_POS,pres,any_of(c("house","senate","senat","congress","aspi_usa"))),
                       by=c("g105"="techq"),suffix=c("",".r"))
cat("\nmein pres vs ref pres exakt:", round(mean(abs(u$pres-u$pres.r)<1e-3,na.rm=TRUE),3),"\n")
if("house.r"%in%names(u)) cat("house exakt:",round(mean(abs(u$house-u$house.r)<1e-2,na.rm=TRUE),3),"\n")
sr <- if("senate.r"%in%names(u))"senate.r" else if("senat.r"%in%names(u))"senat.r" else NA
if(!is.na(sr)) cat("senate exakt:",round(mean(abs(u$senate-u[[sr]])<1e-2,na.rm=TRUE),3),"\n")
cat("\nBeispielzeilen (2010er):\n")
print(u|>filter(g105>=200,g105<=210)|>select(g105,house,senate,congress,pres,GOV_POS,GOV_POS.r)|>as.data.frame(),row.names=FALSE)
