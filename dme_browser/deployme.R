library(rsconnect)
deployApp(appDir=getwd(),appFiles=c("app.R","rNA_flowcells.Rmd","download_tools.R","www/blank.html"))

