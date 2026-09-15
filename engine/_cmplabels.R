libpath <- "C:/Users/User/Documents/R/win-library/4.6"; .libPaths(c(libpath, .libPaths()))
suppressMessages({library(haven)})
mp <- "D:/Downloads/MPDataset_MPDS2025a_stata14.dta"
codes <- strsplit(readLines("K:/DATEIEN/Data/PIP2022/ASPM_WebApp/app/data/per_codes.txt")[1], ",")[[1]]
if(file.exists(mp)){
  d <- read_dta(mp, n_max=1)
  labs <- sapply(names(d), function(v){ l<-attr(d[[v]],"label"); if(is.null(l)) "" else l })
  found <- 0; missing <- c()
  con <- file("K:/DATEIEN/Data/PIP2022/ASPM_WebApp/app/data/per_labels.csv","w", encoding="UTF-8")
  writeLines("code,label", con)
  for(cd in codes){
    lab <- if(cd %in% names(labs)) labs[[cd]] else ""
    if(nzchar(lab)){ found<-found+1 } else { missing<-c(missing,cd) }
    lab2 <- gsub('"','',lab); if(grepl(",",lab2)) lab2<-paste0('"',lab2,'"')
    writeLines(paste0(cd,",",lab2), con)
  }
  close(con)
  cat("MPDS gefunden. Labels vorhanden fuer", found, "von", length(codes), "Codes.\n")
  if(length(missing)) cat("OHNE Label:", paste(missing,collapse=" "), "\n")
} else {
  cat("MPDS NICHT gefunden unter", mp, "\n")
}
