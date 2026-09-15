libpath <- "C:/Users/User/Documents/R/win-library/4.6"; .libPaths(c(libpath, .libPaths()))
suppressMessages({library(haven); library(dplyr)})
a18 <- "K:/DATEIEN/Data/PIP2022/_ARCHIV/_PIP_COLLECTION_v2018-02/03 pip_analysis/Framework"
d <- read_dta(file.path(a18,"datasets/pip_ts_aspm.dta")) |> as.data.frame()
x <- d |> filter(g105==150, !is.na(p614)) |> select(g101,p101,p201,p614,ja10f)
cat("g105=150 Zeilen mit p614 nicht-NA (",nrow(x),"):\n"); print(head(x,20), row.names=FALSE)
cat("\nLaender mit p614==1:", paste(unique(d$g101[d$g105==150 & d$p614==1 & !is.na(d$p614)]),collapse=","), "\n")
cat("p614==1 & p201==1 (PM-Zeile):", sum(d$g105==150 & d$p614==1 & d$p201==1, na.rm=TRUE),
    " | p614==1 gesamt:", sum(d$g105==150 & d$p614==1, na.rm=TRUE), "\n")
# ist p614 auf ALLEN Parteizeilen des Praesidentschaftslandes oder nur einer?
pc <- unique(d$g101[d$g105==150 & d$p614==1 & !is.na(d$p614)])
if(length(pc)) { cat("\nPraesidentschaftsland",pc[1],"g105=150 alle Zeilen p614/p201/ja10f:\n")
  print(d|>filter(g105==150,g101==pc[1])|>select(p101,p201,p614,ja10f)|>as.data.frame(),row.names=FALSE) }
