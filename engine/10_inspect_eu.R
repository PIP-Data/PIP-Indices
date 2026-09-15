libpath <- "C:/Users/User/Documents/R/win-library/4.6"; .libPaths(c(libpath, .libPaths()))
suppressMessages({library(haven); library(dplyr)})
fw <- "K:/DATEIEN/Data/PIP2022/_PIP_COLLECTION_v2022-03/03 pip_analysis/ASPM_Extended_Replication_2022/Framework"
e <- read_dta(file.path(fw,"datasets/external_data.dta")) |> as.data.frame()
cat("== external_data:", nrow(e),"x",ncol(e),"==\n")
cat("Spalten:", paste(names(e), collapse=", "), "\n\n")
d <- read_dta(file.path(fw,"datasets/pip_ts_aspm.dta")) |> as.data.frame()
# EU-relevante p-Variablen: Kommissare, MdEP, Ratsstimmgewichte, EU-Praesidentschaft
eucols <- grep("^p6", names(d), value=TRUE)
cat("== p6xx-Spalten (EU) ==\n", paste(eucols, collapse=", "), "\n\n")
# Deutschland ein EU-Quartal: welche p6xx sind gesetzt
g <- d |> filter(g101==276, g105==200)
cat("GER g105=200 p6xx-Werte (nicht alle NA):\n")
nn <- eucols[sapply(eucols, function(c) any(!is.na(g[[c]])))]
for(c in nn) cat(sprintf("  %-8s %s\n", c, paste(round(head(g[[c]][!is.na(g[[c]])],4),2),collapse=",")))
# 2018-Referenzpaar checken
a18 <- "K:/DATEIEN/Data/PIP2022/_ARCHIV/_PIP_COLLECTION_v2018-02/03 pip_analysis/Framework"
if(file.exists(file.path(a18,"aspm_lr_eu_quarterly.dta"))){
  r <- read_dta(file.path(a18,"aspm_lr_eu_quarterly.dta"))
  cat("\n2018-EU-Referenz:", nrow(r),"x",ncol(r)," Spalten:", paste(grep("POS|iso|techq|year",names(r),value=TRUE),collapse=", "),"\n")
}
