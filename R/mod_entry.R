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
      col_2(
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
      col_2(
        checkboxInput(
          ns("edit_table"),
          "Edit table",
          value = FALSE
        )
      )
    ),
    fluidRow(
      col_2(
        checkboxInput(
          ns("screenshot_only"),
          "Upload screenshot only",
          value = FALSE
        )
      ),
      col_2(
        checkboxInput(
          ns("data_only"),
          "Upload data only",
          value = FALSE
        )
      ),
      col_2(
        checkboxInput(
          ns("overwrite_table"),
          "Overwrite current table",
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
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Entry",
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
      ),
      tabPanel(
        "Cumulative",
        fluidRow(
          col_3(
            actionButton(
              ns("refresh_full_data"),
              "Refresh",
              icon = icon("reload"),
              class = "btn-success"
            )
          ),
          col_3(
            checkboxInput(
              ns("edit_full_table"),
              "Edit full table",
              value = FALSE
            )
          ),
          col_3(
            actionButton(
              ns("upload_full_data"),
              "Re-Upload",
              icon = icon("upload"),
              class = "btn-success"
            )
          ),
          col_3(
            downloadButton(
              ns("download_full"),
              "Download Full"
            )
          )
        ),
        fluidRow(
          col_12(
            conditionalPanel(
              condition = "input.edit_full_table",
              ns = ns,
              excelR::excelOutput(ns("table_full_edit_ui"))
            ),
            tableOutput(ns("cumulative_table"))
          )
        )
      )
    )
  )
}
    
#' entry Server Function
#' @import dplyr
#' @import excelR
#' @import pins
#' @noRd 
mod_entry_server <- function(input, output, session, meta_df){
  ns <- session$ns
  
  # reactive values
  table_edit_rv <- reactiveVal(NULL)
  table_full_edit_rv <- reactiveVal(NULL)
  upload_trigger <- reactiveVal(NULL)
  
  # obtain path to uploaded file after fix
  image_path <- reactive({
    req(input$image_upload)
    res <- fixUploadedFilesNames(input$image_upload)
    return(res$datapath)
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
  
  observeEvent(input$table_edit_ui, {
    table_data <- excelR::excel_to_R(input$table_edit_ui)
    if (!is.null(table_data)) {
      # workaround to get column names back to normal
      names(table_data) <- c("position", "player_name", "player_time", "points")
      
      # reconvert key variable to correct format
      table_data <- table_data %>%
        mutate(position = as.integer(position), points = as.numeric(points))
      
      table_edit_rv(table_data)
    }
  })
  
  # display contents of table
  output$table <- renderTable({
    req(meta_final())
    dplyr::select(meta_final(), position, player_name, player_time, points,
                  season, track, direction, driver, car, date)
  })
  
  output$table_full_edit_ui <- excelR::renderExcel({
    if (is.null(table_full_edit_rv())) {
      df <- pins::pin_get("hotshots_race_results", board = "dospace", cache = FALSE)
    } else {
      df <- table_full_edit_rv()
    }
    
    excelTable(
      data = df,
      columns = tibble::tibble(
        title = c("Season", "Driver", "Car", "Track", "Direction", "Grand Prix", "Date", "Position", "Name", "Time", "Points", "Link"),
        width = rep(100, 12),
        type = c('text', 'text', 'text', 'text', 'text', 'text', 'calendar', 'text', 'text', 'text', 'text', 'text')
      ),
      columnSorting = FALSE,
      rowDrag = FALSE,
      allowInsertRow = TRUE,
      allowInsertColumn = FALSE,
      allowDeleteRow = TRUE,
      allowDeleteColumn = FALSE,
      allowRenameColumn = FALSE
    )
  })
  
  observeEvent(input$table_full_edit_ui, {
    table_data <- excelR::excel_to_R(input$table_full_edit_ui)
    if (!is.null(table_data)) {
      # workaround to get column names back to normal
      names(table_data) <- c("season", "driver", "car", "track", "direction", "grand_prix", "date", "position", "player_name", "player_time", "points", "screenshot_link")
      
      # reconvert key variable to correct format
      table_data <- table_data %>%
        mutate(position = as.integer(position), points = as.numeric(points))
      
      table_full_edit_rv(table_data)
    }
  })
  
  output$cumulative_table <- renderTable({
    #req(upload_trigger())
    x <- upload_trigger()
    if (is.null(table_full_edit_rv())) {
      df <- pins::pin_get("hotshots_race_results", board = "dospace", cache = FALSE)
    } else {
      df <- table_full_edit_rv()
    }
    df
  })
  
  observeEvent(input$upload_full_data, {
    browser()
    if (is.null(table_full_edit_rv())) {
      df <- pins::pin_get("hotshots_race_results", board = "dospace", cache = FALSE)
    } else {
      df <- table_full_edit_rv()
    }
    note_id <- showNotification("Upload in progress", type = "default", duration = NULL)
    
    pin(df, "hotshots_race_results", board = "dospace")
    pin(df, "hotshots_race_results", board = "hotshots_github")
    pin(df, "hotshots_race_results", board = "local")
    
    removeNotification(note_id)
    showNotification("Data uploaded!", type = "message", duration = 2)
  })
  
  # upload data to pins repo
  observeEvent(input$upload_data, {
    req(meta_final())
    
    note_id <- showNotification("Upload in progress", type = "default", duration = NULL)
    # generate new file name of image file
    img_new_filename <- glue::glue("{season}-{date}-{grand_prix}-{track}-{direction}",
                                   season = unique(meta_final()$season),
                                   date = unique(meta_final()$date),
                                   grand_prix = unique(meta_final()$grand_prix),
                                   track = unique(meta_final()$track),
                                   direction = unique(meta_final()$direction))
    
    # make copy of image file with new name with smaller resolution
    new_image <- copy_race_image(image_path(), img_new_filename)
    image_ext <- fs::path_ext(new_image)

    # new_path <- fs::path(fs::path_dir(image_path()), paste0(img_new_filename, ".png"))
    # fs::file_copy(image_path(), new_path)
    
    # create link to where image file is stored in GitHub pin
    img_link <- glue::glue("https://raw.githubusercontent.com/rpodcast/hotshots.pinboard/master/{img_new_filename}/{img_new_filename}.{image_ext}")
    
    # send image file to pin board
    if (!input$data_only) {
      pin(new_image, name = img_new_filename, board = "hotshots_github")
      pin(new_image, name = img_new_filename, board = "dospace")
      pin(new_image, name = img_new_filename, board = "local")
    }
    
    
    # add image link to metadata
    meta_final2 <- dplyr::mutate(meta_final(), screenshot_link = img_link)
    
    if (!input$overwrite_table) {
      if (pin_exists("hotshots_race_results", board = "dospace")) {
        df_all <- pins::pin_get("hotshots_race_results", board = "dospace", cache = FALSE)
        df <- dplyr::bind_rows(df_all, meta_final2)
      } else {
        df <- meta_final2
      }
    } else {
      df <- meta_final2
    }
    
    # send pin to board
    if (!input$screenshot_only) {
      pin(df, "hotshots_race_results", board = "dospace")
      pin(df, "hotshots_race_results", board = "hotshots_github")
      pin(df, "hotshots_race_results", board = "local")
      
      pin(meta_final2, "hotshots_race_results_latest", board = "dospace")
      pin(meta_final2, "hotshots_race_results_latest", board = "hotshots_github")
      pin(meta_final2, "hotshots_race_results_latest", board = "local")
    }
    
    
    removeNotification(note_id)
    upload_trigger(rnorm(1))
    showNotification("Data uploaded!", type = "message", duration = 2)
  })
  
  output$download_full <- downloadHandler(
    filename = function() {
      glue::glue("{season}-fulldata.csv", season = unique(meta_final()$season))
    },
    content = function(file) {
      browser()
      df_all <- pins::pin_get("hotshots_race_results", board = "dospace", cache = FALSE)
      readr::write_csv(df_all, file)
    }
  )
}
    
## To be copied in the UI
# mod_entry_ui("entry_ui_1")
    
## To be copied in the server
# callModule(mod_entry_server, "entry_ui_1")
 
