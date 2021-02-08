#' entry UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_entry_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_3(
        fileInput(
          ns("image_upload"),
          "Upload screenshot",
          multiple = FALSE,
          accept = "image/*"
        )
      ),
      col_3(
        sliderInput(
          ns("n_racers"),
          "Number of Racers",
          min = 1,
          max = 8,
          value = 8,
          step = 1
        )
      )
    ),
    fluidRow(
      col_6(
        plotOutput(ns("preview"))
      ),
      col_6(
        tableOutput(ns("table"))
      )
    )
  )
}
    
#' entry Server Function
#' @import dplyr
#' @noRd 
mod_entry_server <- function(input, output, session, meta_df){
  ns <- session$ns
  
  # obtain path to uploaded file after fix
  image_path <- reactive({
    req(input$image_upload)
    res <- fixUploadedFilesNames(input$image_upload)
    return(res$datapath)
  })
  
  # assemble input to processing functions
  meta_input <- reactive({
    req(image_path())
    req(meta_df())
    
    res <- dplyr::mutate(meta_df(), import_file = image_path(), .before = 1)
    
    return(res)
  })
  
  output$preview <- renderPlot({
    req(image_path())
    view_race_image(image_path())
  })
  
  # reactive for processed race results data frame
  meta_extract <- reactive({
    req(image_path())
    req(input$n_racers)
    
    # assemble input tibble
    df <- tibble::tibble(
      import_file = image_path()
    )
    
    res <- df %>%
      mutate(
        standings_table = list(
          import_race_image(
            import_file = import_file, 
            n_drivers = input$n_racers, 
            view_image = FALSE
          )
        )
      )
    
    return(res)
  })
  
  # asssemble final set
  meta_final <- reactive({
    req(meta_df())
    req(meta_extract())
    
    res <- dplyr::bind_cols(meta_df(), meta_extract()) %>%
      tidyr::unnest(cols = "standings_table")
    
    return(res)
  })
  
  output$table <- renderTable({
    req(meta_final())
    dplyr::select(meta_final(), position, player_name, player_time,
                  track, direction, driver, car)
  })
  
  
  
}
    
## To be copied in the UI
# mod_entry_ui("entry_ui_1")
    
## To be copied in the server
# callModule(mod_entry_server, "entry_ui_1")
 
