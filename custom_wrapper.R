# App-Wrapper fuer Custom-Index in WebR. Erwartet global: custom_index (per***-Spalten je Partei/Quartal).
# formula: R-Arithmetik auf den per-Spalten, z.B. "per501 - per110"; isoNums: Laendercodes (g101).
ci_run <- function(formula, isoNums){
  sub <- custom_index[custom_index$iso %in% as.numeric(isoNums), , drop=FALSE]
  idx <- with(sub, eval(parse(text=formula)))          # Formel im Datenframe-Kontext auswerten
  if(length(idx) != nrow(sub)) idx <- rep_len(idx, nrow(sub))
  out <- data.frame(iso=sub$iso, year=sub$year, quarter=sub$quarter, techq=sub$techq,
                    party=sub$party, name=sub$name, INDEX=round(as.numeric(idx),4),
                    stringsAsFactors=FALSE)
  out <- out[!is.na(out$INDEX), ]
  out <- out[order(out$iso, out$party, out$techq), ]
  con <- textConnection("._c","w"); on.exit(close(con))
  utils::write.csv(out, con, row.names=FALSE, na="")
  paste(textConnectionValue(con), collapse="\n")
}

# ===== Custom-Index als 4. Dimension (Country/EU): Formel auf per*-Spalten -> ideo-Spalte =====
# Erwartet global: custom_per (id + per101..per706, ALLE Zeilen von d/eu_data, s. custom_per.rds).

# Gleiche Validierung wie die JS-Regex im UI (^[\sper0-9+\-*/().]+$) - Verteidigung in der Tiefe,
# falls ci_run/aspm_query_custom je ohne den JS-Check aufgerufen werden.
eval_custom_formula <- function(df, formula){
  if(!grepl("^[[:space:]per0-9+*/().-]+$", formula)) stop("formula may only use perNNN, numbers and + - * / ( )")
  with(df, eval(parse(text=formula)))
}

# per*-Spalten einmalig per id an d (und, falls schon geladen, eu_data) anhaengen. Danach reicht
# eval_custom_formula(d, formula) direkt - kein erneuter Join noetig.
ensure_custom_per_joined <- function(){
  if(!"per101" %in% names(d)){
    d <<- merge(d, custom_per, by="id", all.x=TRUE, sort=FALSE)
  }
  if(exists("eu_data", inherits=TRUE) && !("per101" %in% names(eu_data))){
    eu_data <<- merge(eu_data, custom_per, by="id", all.x=TRUE, sort=FALSE)
  }
}

# Country: Custom-Dimension-Lauf (identische Signatur/Rueckgabe wie aspm_query, nur mit Formel statt ideo)
aspm_query_custom <- function(isos, specs, formula, gran="quarterly", eu_pos=NULL){
  ensure_custom_per_joined()
  d$._custom <<- eval_custom_formula(d, formula)
  aspm_query(isos, specs, ideo="._custom", gran=gran, eu_pos=eu_pos)
}

# EU: Custom-Dimension-Lauf. Braucht (wie govpos_eu.rds beim Praecompute) erst die GOV_POS je Land/
# Quartal auf der Custom-Formel (Default-Specs aller Laender), dann estimate_eu darauf.
# all_isos/all_specs: Laendercodes+Default-Config-Strings ALLER Laender (nicht nur ausgewaehlte),
# da EU-Institutionen die GOV_POS jedes (Ex-)Mitgliedslands brauchen. Erwartet eu_wrapper.R (eu_run)
# und ensureEU()-Daten (eu_data/external_eu/govpos_eu) bereits geladen.
eu_run_custom <- function(formula, all_isos, all_specs, council="default", commission="default",
                          councilofmin="default", euparl="default", principal="1", portf=NULL,
                          portf_isos=""){
  ensure_custom_per_joined()
  d$._custom <<- eval_custom_formula(d, formula)
  eu_data$._custom <<- eval_custom_formula(eu_data, formula)
  sp <- as.list(as.character(all_specs)); names(sp) <- as.character(all_isos)
  gp <- estimate_aspm(d, sp, ideo="._custom", referenda=get0("referenda"), verbose=FALSE)$quarterly
  gp <- gp[, c("iso","techq","GOV_POS")]
  eu_run("._custom", council, commission, councilofmin, euparl, principal, gp_override=gp,
         portf=portf, portf_isos=portf_isos)
}
