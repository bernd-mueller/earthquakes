library(grid)
library(ggplot2)

#' Function that constructa the grid objects for the layera from "geom_timeline".
#'
#' @return A grid object list ready to be added to a ggplot layer.
#'
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 draw_key_point
#' @importFrom ggplot2 Geom
#' @importFrom ggplot2 ggproto
#' @importFrom grid circleGrob
#' @importFrom grid gpar
#' @importFrom grid polylineGrob
#' @importFrom grid gList

GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
                                 required_aes = c("x", "size", "colour"),
                                 default_aes = ggplot2::aes(y = 0.3, colour = "grey", size = 1.0,
                                                            alpha = 0.6, shape = 21, fill = "grey",
                                                            stroke = 1.0),
                                 draw_key = ggplot2::draw_key_point,
                                 draw_group = function(data, panel_scales, coord) {
                                   coords <- coord$transform(data, panel_scales)
                                   points <- grid::circleGrob(
                                     x = coords$x, y = coords$y,
                                     #r = (sqrt(coords$size / pi)) / 65, # is size aesthetic based on area?
                                     #r = coords$size / 150,
                                     r = (2 ^ coords$size) / 1000, # Logarithmic value?
                                     gp = grid::gpar(
                                       fill = coords$colour,
                                       col = coords$colour,
                                       alpha = coords$alpha
                                     ))

                                   y_line_center <- unique(coords$y)

                                   lines <- grid::polylineGrob(
                                     x = unit(c(0, 1), "npc"),
                                     y = unit(c(y_line_center, y_line_center), "npc"),
                                     gp = grid::gpar(col = "grey")
                                   )

                                   return(grid::gList(points, lines))
                                 })

#' This method creates the required layer containing the timeline geom for earthquake data on.
#' The color aesthetic shows the number of deaths.
#' The size aesthetic shows the Richter scale of the earthquakes.
#'
#' @param mapping Set of aesthetic mappings created by aes or aes_.
#' @param data The data to be displayed in this layer, data.frame
#' @param stat The statistical transformation to use on the data for this layer, as a string.
#' @param position Position adjustment as a string or function.
#' @param na.rm either TRUE or FALSe for for removing missing values with or without warning.
#' @param show.legend TRUE or FALSE to show the legend or notlogical.
#' @param inherit.aes TRUE or FALSE for inheriting aies
#' @param ... Additional parameters
#'
#' @return A ggplot2 layer containing the timeline geom for earthquake data.
#'
#' @export
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ggplot_build
#' @importFrom ggplot2 ggplot_gtable
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 layer
#' @importFrom grid grid.draw
#' @importFrom readr read_delim
#'
#' @examples
#' \dontrun {
#'  p <- readr::read_delim(file = "data/signif.txt", delim = "\t") %>%
#'      eq_clean_data() %>% eq_location_clean() %>%
#'      dplyr::filter(YEAR >= 2000, COUNTRY %in% "USA") %>%
#'      ggplot2::ggplot() +
#'      geom_timeline(ggplot2::aes(x = DATE, colour = DEATHS,
#'        size = EQ_MAG_ML)) +
#'      ggplot2::labs(x = "DATE", color = "Deaths", size = "Richter scale")
#'  gt <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(p))
#'  grid::grid.draw(gt)
#' }
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimeline, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' This method returns a layer that contains the timeline geom with labels for earthquake locations.
#' The color aesthetic shows the number of deaths.
#' The size aesthetic shows the Richter scale of the earthquakes.
#'
#' @param mapping Set of aesthetic mappings created by aes or aes_.
#' @param data The data to be displayed in this layer, data.frame
#' @param stat The statistical transformation to use on the data for this layer, as a string.
#' @param position Position adjustment as a string or function.
#' @param na.rm either TRUE or FALSe for for removing missing values with or without warning.
#' @param show.legend TRUE or FALSE to show the legend or notlogical.
#' @param inherit.aes TRUE or FALSE for inheriting aies
#' @param ... Additional parameters
#'
#' @return A layer containing the timeline geom with labels.
#'
#' @export
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 ggplot_gtable
#' @importFrom grid grid.draw
#' @importFrom readr read_delim
#'
#' @examples
#' \dontrun {
#'  p <- readr::read_delim(file = system.file("extdata", "signif.txt", package="noaa"),
#'                                           delim = "\t") %>%
#'      eq_clean_data() %>% eq_location_clean() %>%
#'      dplyr::filter(YEAR >= 1900, !is.na(DEATHS), !is.na(EQ_MAG_ML),
#'                    COUNTRY %in% c("CHINA", "USA")) %>%
#'      ggplot2::ggplot(ggplot2::aes(x = DATE,
#'        colour = DEATHS,
#'        size = EQ_MAG_ML)) +
#'      geom_timeline() +
#'      geom_timeline_label(ggplot2::aes(label = LOCATION_NAME, n_max = 5)) +
#'      ggplot2::labs(x = "DATE", color = "# deaths", size = "Richter scale value")
#'  gt <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(p))
#'  grid::grid.draw(gt)
#' }
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                                position = "identity", na.rm = FALSE,
                                show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimelineLabel, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' Function used internally to construct grid objects timeline geom with labels.
#'
#' @return A grid object list ready to be added to a ggplot layer.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr top_n
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 Geom
#' @importFrom ggplot2 draw_key_polygon
#' @importFrom grid textGrob
#' @importFrom grid polylineGrob
#' @importFrom grid gpar
#' @importFrom grid gList
GeomTimelineLabel <- ggplot2::ggproto("GeomTimelineLabel", ggplot2::Geom,
                                      required_aes = c("x", "label", "size"),
                                      default_aes = ggplot2::aes(y = 0.3, n_max = 10000, stroke = 1.0,
                                                                 size = 1.0, colour = "grey",
                                                                 fill = "grey"),
                                      draw_key = ggplot2::draw_key_polygon,
                                      draw_group = function(data, panel_scales, coord) {
                                        data <- data %>% top_n(n = as.integer(data$n_max[1]), size)
                                        coords <- coord$transform(data, panel_scales)
                                        offset <- 0.05
                                        names <- grid::textGrob(
                                          label = coords$label,
                                          x = unit(coords$x, "npc"),
                                          y = unit(coords$y + offset, "npc"),
                                          just = c("left", "bottom"),
                                          gp = grid::gpar(fontsize = 10, col = 'black'),
                                          rot = 45
                                        )

                                        lines <- grid::polylineGrob(
                                          x = unit(c(coords$x, coords$x), "npc"),
                                          y = unit(c(coords$y, coords$y + offset), "npc"),
                                          id = rep(1:length(coords$x), 2),
                                          gp = grid::gpar(
                                            col = "grey"
                                          )
                                        )

                                        return(grid::gList(names, lines))
                                      })



