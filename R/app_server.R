#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  
  waiter::waiter_hide()
  sever::sever(disconnected, bg_color = "#FF6633")
  
  # List the first level callModules here
  callModule(mod_national_server, "national_ui_1")
}
