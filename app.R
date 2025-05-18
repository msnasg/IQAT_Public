

# ---------------------------------------------------------------------------- #
# IQAT shiny app.R
# ---------------------------------------------------------------------------- #
# rm(list=ls(all=TRUE)); gc() # Free Environment

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# # No Need for Deploy

# shiny::shinyApp(
#   onStart = function(){
#     pathwd_dash <<- getwd()
#     if(base::dir.exists(paste0(pathwd_dash, "/R"))){
#       base::suppressPackageStartupMessages({ 
#         base::suppressWarnings({
#           sourceEntireFolder("./R/utils")
#           sourceEntireFolder(paste0(pathwd_dash, "/R/Modules"))
#           sourceEntireFolder(paste0(pathwd_dash, "/R/Modules/Markets"))
#           sourceEntireFolder(paste0(pathwd_dash, "/R/Modules/Analytics"))
#           sourceEntireFolder(paste0(pathwd_dash, "/R/Modules/DataSets"))
#           sourceEntireFolder(paste0(pathwd_dash, "/R/Modules/Trading_Models"))
#           sourceEntireFolder("./R/Modules/Fixed_Income")
#           sourceEntireFolder(paste0(pathwd_dash, "/R"))
#           sourceEntireFolder("./R/Modules/TimeSeries/Regression")
#           source(paste0(pathwd_dash, "/ui.R"))
#           source(paste0(pathwd_dash, "/server.R"))
#           
#         }) 
#       })
#     }     
#     onStop(function(){
#       #clean_up_list <- c("tmp")
#       suppressWarnings(
#         suppressMessages({
#           #res <- try(sapply(clean_up_list, function(x) if (exists(x, envir = .GlobalEnv)) rm(list = x, envir = .GlobalEnv)), silent = TRUE)
#           #rm(res)
#           print("onStop")
#         })
#       )
#       message(paste("Session stopped at ", Sys.time()))
#       stopApp()
#     })
#   }, # onStart
#   ui = app_ui, 
#   server = shiny::shinyServer(app_server))

# ---------------------------------------------------------------------------- #
# End
# ---------------------------------------------------------------------------- #