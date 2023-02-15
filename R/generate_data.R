library(magrittr)
library(dplyr)
library(grDevices)

map <- list()

#' Complete list of palettes
#'
#' Use \code{\link{witcher}} to construct palettes of desired length.
#'
#' @export
#'

sydskog_palettes <- list(
  slu = c("black","green", "blue"),
  not_slu=  c("#12141F", "#4e0002", "#9D0005", "darkgreen", "lightgreen"),
  oak =  c("#382a1d", "#98632c", "#816e5d", "#c1af98", "#6f5943")
)


#' Expand list of palettes
#'
#' @keywords internal
#'

# Expand palette to accept contiuous scales or longer discrete scales
complete_palette <- function(option, n = 3e3) {
  complete_col <- c()
  for (i in 1:(length(option) - 1)) {
    cols <- colorRampPalette(c(option[i], option[i + 1]))
    complete_col <- c(complete_col, cols(n))
  }
  return(complete_col)
}


#' make map of colors
#'
#' @importFrom grDevices colorRampPalette n2mfrow
#' @importFrom graphics image par text
#' @keywords internal
#'

# Build DF map
make_map <- function(palettes, option_name) {
  palettes[[option_name]] %>%
    complete_palette() %>%
    grDevices::col2rgb() %>%
    t() %>%
    as.data.frame() %>%
    dplyr::rename(V1 = red) %>%
    dplyr::rename(V2 = green) %>%
    dplyr::rename(V3 = blue) %>%
    dplyr::mutate(option = option_name)
}

for (h in names(sydskog_palettes)) {
  df <- make_map(sydskog_palettes, h)
  map <- rbind(map, df)
}


usethis::use_data(map, internal = TRUE, overwrite = TRUE)
usethis::use_data(sydskog_palettes, overwrite = TRUE)
usethis::use_pipe(export = TRUE)
utils::globalVariables(c("red", "green", "blue", "option_name"))
