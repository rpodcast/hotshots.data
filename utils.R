#' @import magick
#' @import tesseract
#' @import dplyr
#' @import ggplot2
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


# function to ingest data
#' @import dplyr
#' @import magick
#' @import tesseract
#' @import purrr
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
        select(position, player_name, player_time)

    return(df2)

}