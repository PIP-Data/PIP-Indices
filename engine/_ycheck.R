libpath <- "C:/Users/User/Documents/R/win-library/4.6"; .libPaths(c(libpath, .libPaths()))
suppressMessages(library(dplyr))
app <- "K:/DATEIEN/Data/PIP2022/ASPM_WebApp/app"
source(file.path(app,"engine.R")); source(file.path(app,"wrapper.R"))
d <- readRDS(file.path(app,"data","pip_ts_aspm.rds"))
out <- estimate_aspm(d, list("276"="gov(minister:p215,p213) minority(yes;antisys:no) vp(gov,2ch)"),
                     ideo="ja10f", verbose=FALSE)
y <- out$yearly[out$yearly$year==2020, ]
cat(sprintf("ENGINE yearly GER 2020: GOV_POS=%.4f median1st=%.4f median2nd=%.4f eu=%s VP_RANGE=%.4f\n",
    y$GOV_POS[1], y$median1st[1], y$median2nd[1], y$eu[1], y$VP_RANGE[1]))
