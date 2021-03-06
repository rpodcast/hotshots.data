#' @import magick
#' @import tesseract
#' @import dplyr
#' @import ggplot2
#' @noRd
get_data <- function(crop_dim, fuzz, raw_img, combo, view_cell = FALSE) {
    img_p <- raw_img %>%
        image_crop(crop_dim) %>%
        image_quantize(colorspace = "gray") %>%
        image_transparent(color = "gray", fuzz = fuzz) %>%
        image_background("black") %>%
        image_negate()

    val <- ocr(img_p, engine = combo)

    if (view_cell) print(image_ggplot(img_p))

    return(val)
}

#' @import magick
#' @import ggplot2
#' @noRd
view_race_image <- function(import_file) {
    raw_img <- image_read(import_file)
    res <- image_ggplot(raw_img)
    return(res)
}

#' @import magick
copy_race_image <- function(import_file, new_filename = NULL, resolution = "1920x1080") {
    raw_img <- image_read(import_file)
    
    # grab file extension
    file_ext <- fs::path_ext(import_file)
    
    # use current file name if a new one is not specified
    if (is.null(new_filename)) {
        # grab file name without extension
        new_filename <- fs::path_file(import_file)
    }
    
    # scale image
    new_img <- image_scale(raw_img, resolution)
    
    new_path <- fs::path(fs::path_dir(import_file), paste0(new_filename, ".", file_ext))
    image_write(new_img, path = new_path)
    
    message(glue::glue("new image file: {new_path}"))
    return(new_path)
}

#' @import dplyr
#' @import magick
#' @import tesseract
#' @import purrr
#' @noRd
import_race_image <- function(import_file, n_drivers = 8, save_image = FALSE, view_image = FALSE, view_cell = FALSE) {
    raw_img <- image_read(import_file)

    if (view_image) print(image_ggplot(raw_img))

    if (save_image) {
        # grab file name without extension
        root_filename <- fs::path_file(import_file)
        root_dir <- fs::path(dir(import_file))
        comp_img <- image_scale(raw_img, "1920x1080")
        comp_filename <- paste0(root_filename, "_compressed.png")
        image_write(comp_img, path = fs::path(root_dir, comp_filename), format = "png")
    }

    # set ocr options
    combo <- tesseract::tesseract(
        options = list(
        tessedit_char_whitelist = paste0(
            c(letters, LETTERS, " ", ":.0123456789 (-)"), collapse = "")
        )
    )
    # define crop dimensions for name and time slots in table image
    
    dim_df <- tibble::tribble(
    ~position, ~crop_name, ~crop_time, ~fuzz_name, ~fuzz_time,
    1, "700x120+1800+320", "370x120+3300+320", 30, 10,
    2, "700x120+1800+505", "370x120+3280+500", 30, 10, 
    3, "700x120+1780+670", "370x120+3250+660", 30, 10, 
    4, "700x120+1750+830", "370x120+3230+830", 10, 10, 
    5, "700x120+1730+1000", "370x120+3210+990", 10, 10, 
    6, "700x120+1700+1160", "370x120+3160+1160", 10, 10, 
    7, "700x120+1670+1320", "370x120+3140+1320", 10, 10,
    8, "700x120+1640+1480", "370x120+3130+1480", 10, 10
    )
    
    # if less than 8 drivers, remove last rows
    if (n_drivers < 8) {
        dim_df <- dplyr::slice(dim_df, 1:n_drivers)
    }

    # begin extraction
    df2 <- dim_df %>%
        mutate(player_name = purrr::map2_chr(crop_name, fuzz_name, ~get_data(crop_dim = .x, fuzz = .y, raw_img, combo, view_cell = view_cell))) %>%
        mutate(player_time = purrr::map2_chr(crop_time, fuzz_time, ~get_data(crop_dim = .x, fuzz = .y, raw_img, combo, view_cell = view_cell))) %>%
        select(position, player_name, player_time) %>%
        mutate(position = as.numeric(position)) %>%
        mutate_at(c("player_name", "player_time"), ~stringr::str_replace_all(., "\\n", ""))
    
    # merge in points data
    df2 <- left_join(df2, hotshot_points, by = "position")
    df2 <- mutate(df2, points = ifelse(player_time == "DNF", 0, points))

    return(df2)
}

#' Retain original file name of uploaded file
#'
#' @param x result of a shiny fileInput call
#'
#' @return string with original file name
#' @export
#'
#' @note see https://github.com/daattali/advanced-shiny/tree/master/upload-file-names
#' @examples 
#' \dontrun{
#' new_path <- fixUploadFileNames(input$my_upload)$datapath
#' }
fixUploadedFilesNames <- function(x) {
    if (is.null(x)) {
        return()
    }
    
    oldNames = x$datapath
    newNames = file.path(dirname(x$datapath),
                         x$name)
    file.rename(from = oldNames, to = newNames)
    x$datapath <- newNames
    x
}

#' @import shiny
#' @import pins
#' @noRd
hotshots_pin_reactive <- function(name, board, interval = 5000, session = NULL, extract = NULL) {
    # custom version of pins::pin_reactive to deal with a pin not being there initially
    
    board_object <- pins::board_get(board)
    
    shiny::reactivePoll(
        intervalMillis = interval,
        session = session,
        checkFunc = function() {
            # check that pin is actually present before determining the changed time
            pin_check <- pins::pin_find(name, board)
            
            if (nrow(pin_check) < 1) {
                return(NULL)
            } else {
                changed_time <- pins:::pin_changed_time(name, board, extract = extract)
                pins:::pin_log("pin_reactive() change time: ", changed_time)
                
                changed_time
            }
        },
        valueFunc = function() {
            # check that pin is actually present before determining the changed time
            pin_check <- pin_find(name, board)
            
            if (nrow(pin_check) < 1) {
                return(NULL)
            } else {
                res <- pins:::pin_get(name, board = board, extract = extract)
                return(res)
            }
        })
}

#' @import pins
#' @noRd
pin_exists <- function(name, board) {
    board_object <- pins::board_get(board)
    pin_check <- pins::pin_find(name, board)
    if (nrow(pin_check) < 1) {
        return(FALSE)
    } else {
        return(TRUE)
    }
}
