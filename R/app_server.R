#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  meta_df <- callModule(mod_metadata_server, "metadata_ui_1")
  
  callModule(mod_entry_server, "entry_ui_1", meta_df)
}
