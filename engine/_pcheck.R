d <- readRDS("K:/DATEIEN/Data/PIP2022/ASPM_WebApp/app/data/pip_ts_aspm.rds")
pn <- d$cmp09_partyname
cat("CLASS", paste(class(pn),collapse="/"), "\n")
cat("N_NONEMPTY", sum(!is.na(pn) & trimws(as.character(pn))!=""), "of", length(pn), "\n")
# hat es haven-Labels?
lab <- attr(pn, "labels")
cat("HAS_LABELS", !is.null(lab), "\n")
# Beispiele: nicht-leere Parteinamen fuer Deutschland
ger <- d[d$g101==276, ]
nm <- as.character(ger$cmp09_partyname)
u <- unique(data.frame(p101=ger$p101, name=nm))
u <- u[!is.na(u$name) & trimws(u$name)!="", ]
cat("GER_UNIQUE_NAMED_PARTIES", nrow(u), "\n")
for(i in seq_len(min(12,nrow(u)))) cat("P", u$p101[i], "=", u$name[i], "\n")
