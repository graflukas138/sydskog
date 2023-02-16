library(ggplot2)
library(usethis)

map <- list()

#' Complete list of palettes
#'
#' Use \code{\link{sydskog}} to construct palettes of desired length.
#'
#' @export
#'

sydskog_palettes <- list(
  aspen = c("#444341", "#BFBDB6", "#354C3F", "#F2DF53"),
  birch = c("#7b521c", "#bb792b", "#f6a844", "#c69a59","#a89274"), 
  blueberry = c("#132833", "#153040", "#1F4762", "#FDA300", "goldenrod2"),
  cowberry = c("#143608", "#17361d", "#4f6857", "#e0ecdd", "#7e2136"),
  heather = c("#660F57", "#663366", "#003366","#5B7290"),
  slu = c("#471871","#653496", "#7B52AE","#74B652","#56941E", "#94C773"),
  palm = c("#fab73d", "#fddca5","#bbc8ba", "#546747", "#2b331f"),
  pine = c("#263E31", "#334A36", "#4C5E46", "#697A50", "#8D9967"),
  poplar = c("#efeedb","#e0deb7","#d0ce94","#c1be70", "#b2ae4c","#8e8b3d","#6a682e","#47451e"),
  spruce = c("#4F2F18", "#C0A983", "#97ACA5", "#5A6F6A", "#314842","#14261E"),
  oak =  c("#382a1d",  "#98632c", "#816e5d", "#c1af98")
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
