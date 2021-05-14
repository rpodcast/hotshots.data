#' metadata UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_metadata_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_2(
        selectInput(
          ns("season"),
          "Select season",
          choices = c("2021_gp1", "2021_gp2"),
          selected = "2021_gp1"
        )
      ),
      col_2(
        dateInput(
          ns("race_date"),
          "Race Date"
        )
      ),
      col_2(
        selectInput(
          ns("grand_prix"),
          "Select Grand Prix",
          choices = names(hotshot_data$grand_prix)
        )
      ),
      col_2(uiOutput(ns("track_placeholder"))),
      col_2(
        radioButtons(
          ns("direction"),
          "Direction",
          choices = c("normal", "mirrored"),
          selected = "normal"
        )
      )
    ),
    fluidRow(
      col_3(
        selectInput(
          ns("driver"),
          "Select Driver",
          choices = names(hotshot_data$drivers)
        )
      ),
      col_3(uiOutput(ns("car_placeholder")))
    )
  )
}
    
#' metadata Server Function
#'
#' @noRd 
mod_metadata_server <- function(input, output, session){
  ns <- session$ns
  
  output$track_placeholder <- renderUI({
    req(input$grand_prix)
    choices <- hotshot_data$grand_prix[[input$grand_prix]]
    
    selectInput(
      ns("track"),
      "Select Track",
      choices = choices
    )
  })
  
  output$car_placeholder <- renderUI({
    req(input$driver)
    choices <- hotshot_data$drivers[[input$driver]][["cars"]]
    choices <- purrr::map_chr(choices, purrr::pluck, "car_name")
    
    selectInput(
      ns("car"),
      "Select Car",
      choices = choices
    )
  })
  
  # assemble return object
  res <- reactive({
    req(input$track)
    req(input$car)
    # driver, car, track, direction, grand_prix, date
    tibble::tibble(
      season = input$season,
      driver = input$driver,
      car = input$car,
      track = input$track,
      direction = input$direction,
      grand_prix = input$grand_prix,
      date = input$race_date
    )
    
  })
  
  res
}
    
## To be copied in the UI
# mod_metadata_ui("metadata_ui_1")
    
## To be copied in the server
# callModule(mod_metadata_server, "metadata_ui_1")
 
