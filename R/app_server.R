#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import pins
#' @noRd
app_server <- function( input, output, session ) {
  # register pin board
  # TODO: Change to github version
  #board_register_local(name = "blah", cache = "prototyping/board_stuff")
  board_register_github(repo = "rpodcast/hotshots.pinboard", name = "hotshots_github", branch = "master")
  board_register_dospace()
  
  # List the first level callModules here
  meta_df <- callModule(mod_metadata_server, "metadata_ui_1")
  
  callModule(mod_entry_server, "entry_ui_1", meta_df)
}
