libpath <- "C:/Users/User/Documents/R/win-library/4.6"; .libPaths(c(libpath, .libPaths()))
suppressMessages({library(haven); library(dplyr)})
fw <- "K:/DATEIEN/Data/PIP2022/ASPM Extended Replication/Release/Framework"
source("K:/DATEIEN/Data/PIP2022/ASPM_WebApp/engine/lib_aspm.R")
d <- read_dta(file.path(fw,"datasets/pip_ts_aspm.dta")) |> as.data.frame()
ref <- read_dta(file.path(fw,"aspm_lr_quarterly.dta")) |> as.data.frame()

a <- d |> filter(g101==40, g105==219) |> select(p101,p103,p118,p303,p402,p502,ja10f)
cat("== Austria g105=219 alle Zeilen (n=",nrow(a),") ==\n",sep=""); print(a, row.names=FALSE)
cat("\nmein wmedian(ja10f,p303) =", round(wmedian(a$ja10f,a$p303),3),
    " | Referenz median1st =", round(ref$median1st[ref$iso==40&ref$techq==219],3), "\n")
cat("mein GOV_POS(seats) =",
    round(with(a, sum(ifelse(!is.na(p103)&p103==1&!is.na(ja10f),ja10f*p303,0))/sum(ifelse(!is.na(p103)&p103==1&!is.na(ja10f),p303,0))),3),
    " | Referenz GOV_POS =", round(ref$GOV_POS[ref$iso==40&ref$techq==219],3),"\n")

# ist ja10f interpoliert oder Anker? Anteil non-NA je Land
cat("\nja10f non-NA-Anteil (Austria):", round(mean(!is.na(d$ja10f[d$g101==40])),3), "\n")
cat("Vergleich: welche ja*-Spalten gibt es?", paste(grep('^ja',names(d),value=TRUE),collapse=", "), "\n")
# range check
cat("ja10f Bereich:", round(range(d$ja10f,na.rm=TRUE),1), " ref median1st Bereich:", round(range(ref$median1st,na.rm=TRUE),1), "\n")
