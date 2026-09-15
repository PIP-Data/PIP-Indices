libpath <- "C:/Users/User/Documents/R/win-library/4.6"; .libPaths(c(libpath, .libPaths()))
suppressMessages({library(haven); library(dplyr)})
a18 <- "K:/DATEIEN/Data/PIP2022/_ARCHIV/_PIP_COLLECTION_v2018-02/03 pip_analysis/Framework"
d <- read_dta(file.path(a18,"datasets/pip_ts_aspm.dta")) |> as.data.frame()
for(v in c("p123","p201","p502","p503","p614","p615","p616","p611")) cat(sprintf("%-6s vorhanden=%s  class=%s\n",v,v%in%names(d),ifelse(v%in%names(d),class(d[[v]])[1],"-")))
# p123 Beispiel (PM-ID) fuer GER
g <- d |> filter(g101==276, p201==1) |> arrange(g105) |> select(g105,p123) |> head(6)
cat("\nGER PM-Zeilen (p123):\n"); print(as.data.frame(g), row.names=FALSE)
# p614/p615 Werte
cat("\np614 Werte:", paste(sort(unique(d$p614)),collapse=","), "\n")
cat("p615 Werte:", paste(sort(unique(d$p615)),collapse=","), "\n")
# 2018 EU-Config presoverpm/rotprespower
cf <- readLines(file.path(a18,"ESTIMATE_EU_ASPM.do"))
cat("\npresoverpm/rotprespower:\n"); cat(grep("presoverpm|rotprespower",cf,value=TRUE)[1:2],sep="\n")
