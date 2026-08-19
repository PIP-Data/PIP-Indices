# App-Wrapper fuer EU-Custom in WebR. Erwartet global: eu_data, external_eu, govpos_eu.
# ideo_col: "ja10f"/"ja20f"/"bu01f" (oder "._custom", s. eu_run_custom); Modelle "default" =
# historische Per-Perioden-Wahl, sonst ein Modell fuer alle Perioden. principal: "1"/"0".
# gp_override: optionales GOV_POS-data.frame(iso,techq,GOV_POS) statt govpos_eu-Lookup (fuer Custom-Dim).
eu_run <- function(ideo_col, council="default", commission="default", councilofmin="default",
                   euparl="default", principal="1", gp_override=NULL){
  spec <- list(principal = principal %in% c("1","TRUE","true",TRUE))
  if(council      != "default") spec$council      <- rep(council,5)
  if(commission   != "default") spec$commission   <- rep(commission,5)
  if(councilofmin != "default") spec$councilofmin <- rep(councilofmin,5)
  if(euparl       != "default") spec$euparl       <- rep(euparl,5)
  gp <- if(!is.null(gp_override)) gp_override else govpos_eu[govpos_eu$dim==ideo_col, c("iso","techq","GOV_POS")]
  out <- estimate_eu(eu_data, ideo_col, gp, external=external_eu, spec=spec)
  qmap <- unique(eu_data[!is.na(eu_data$g105), c("g105","g103","g104")])
  qmap <- qmap[!duplicated(qmap$g105), ]
  out <- merge(out, qmap, by="g105", all.x=TRUE)
  out <- out[order(out$g105), ]
  out <- data.frame(year=out$g103, quarter=out$g104, techq=out$g105,
                    EUCOU_POS=round(out$EUCOU_POS,4), COMM_POS=round(out$COMM_POS,4),
                    COUNCIL_POS=round(out$COUNCIL_POS,4), EP_POS=round(out$EP_POS,4),
                    EU_POS=round(out$EU_POS,4))
  out <- out[!is.na(out$EU_POS), ]
  con <- textConnection("._c","w"); on.exit(close(con))
  utils::write.csv(out, con, row.names=FALSE, na="")
  paste(textConnectionValue(con), collapse="\n")
}
