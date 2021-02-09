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
      ),
      col_3(
        checkboxInput(
          ns("edit_table"),
          "Edit table",
          value = FALSE
        )
      ),
      col_3(
        actionButton(
          ns("upload_data"),
          "Upload",
          icon = icon("upload"),
          class = "btn-success"
        )
      )
    ),
    fluidRow(
      col_6(
        plotOutput(ns("preview"))
      ),
      col_6(
        conditionalPanel(
          condition = "input.edit_table",
          ns = ns,
          excelR::excelOutput(ns("table_edit_ui"))
        ),
        tableOutput(ns("table"))
      )
    )
  )
}
    
#' entry Server Function
#' @import dplyr
#' @import excelR
#' @noRd 
mod_entry_server <- function(input, output, session, meta_df){
  ns <- session$ns
  
  # reactive values
  table_edit_rv <- reactiveVal(NULL)
  
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
    df <- tibble::tibble(import_file = image_path())
    
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
    
    res <- tidyr::unnest(res, cols = "standings_table") %>%
      select(., -import_file)
    
    table_edit_rv(res)
    
    return(res)
  })
  
  # asssemble final set
  meta_final <- reactive({
    req(meta_df())
    req(meta_extract())
    req(table_edit_rv())
    
    res <- dplyr::bind_cols(meta_df(), table_edit_rv())
    
    return(res)
  })
  
  # edit viewer
  output$table_edit_ui <- excelR::renderExcel({
    req(table_edit_rv())
    excelTable(
      data = table_edit_rv(),
      columns = tibble::tibble(
        title = c("Position", "Name", "Time", "Points"),
        width = c(100, 200, 200, 100),
        type = c('text', 'text', 'text', 'text')
      ),
      columnSorting = FALSE,
      rowDrag = FALSE,
      allowInsertRow = FALSE,
      allowInsertColumn = FALSE,
      allowDeleteRow = FALSE,
      allowDeleteColumn = FALSE,
      allowRenameColumn = FALSE
    )
  })
  
  # TODO: Finish
  observeEvent(input$table_edit_ui, {
    table_data <- excelR::excel_to_R(input$table_edit_ui)
    if (!is.null(table_data)) {
      # workaround to get column names back to normal
      names(table_data) <- c("position", "player_name", "player_time", "points")
      
      table_edit_rv(table_data)
    }
  })
  
  # display contents of table
  output$table <- renderTable({
    req(meta_final())
    dplyr::select(meta_final(), position, player_name, player_time, points,
                  track, direction, driver, car)
  })
  
  # upload data to pins repo
  observeEvent(input$upload_data, {
    req(meta_final())
    message("entered here")
  })
  
  
}
    
## To be copied in the UI
# mod_entry_ui("entry_ui_1")
    
## To be copied in the server
# callModule(mod_entry_server, "entry_ui_1")
 
