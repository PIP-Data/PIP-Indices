# App-Wrapper fuer WebR: eine ASPM-Abfrage rechnen und als data.frame zurueckgeben.
# Erwartet, dass engine.R (= lib_aspm.R) bereits geladen ist und `d`/`referenda` im globalen Env liegen.

# isos: character/numeric Vektor der Laendercodes (g101); specs: gleich langer Vektor der Config-Strings
# (z.B. "gov(seats) minority(yes;antisys:no) vp(gov)"); ideo: "ja10f" (LR) / "ja20f" (GG) / "bu01f" (RILE).
# eu_pos: optional data.frame(techq,pos) — EU-Position je Quartal, noetig fuer vp(...,eu)
aspm_query <- function(isos, specs, ideo="ja10f", gran="quarterly", eu_pos=NULL){
  sp <- as.list(as.character(specs)); names(sp) <- as.character(isos)
  out <- estimate_aspm(d, sp, ideo=ideo, referenda=get0("referenda"), eu_pos=eu_pos, verbose=FALSE)
  res <- if(identical(gran,"yearly")) out$yearly else out$quarterly
  res <- as.data.frame(res)
  # Rundung fuer Anzeige/Export, numerische Spalten
  num <- sapply(res, is.numeric); num["iso"] <- FALSE; num["year"] <- FALSE; num["quarter"] <- FALSE; num["techq"] <- FALSE
  res[num] <- lapply(res[num], function(x) round(x, 4))
  res
}

# Als CSV-String (fuer Download im Browser)
aspm_csv <- function(res){
  con <- textConnection("._csv", "w"); on.exit(close(con))
  utils::write.csv(res, con, row.names=FALSE, na="")
  paste(textConnectionValue(con), collapse="\n")
}
