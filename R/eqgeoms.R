library(grid)
library(ggplot2)



geom_timeline_proto <- ggproto("geom_timeline", Geom,
                       required_aes = c("x", "y", "xmin", "xmax"),
                       default_aes = aes(shape = 1, xmin = 2005, xmax = 2010),
                       draw_key = draw_key_point,
                       setup_data = function (data, params) {
                         data <- data[is.finite(data$x), ]

                         # Restrict to dates between xmin and xmax
                         if(!is.null(params$xmin)){
                           data <- data[data$x >= as.numeric(as.Date(params$xmin)), ]
                         }
                         if(!is.null(params$xmax)){
                           data <- data[data$x <= as.numeric(as.Date(params$xmax)), ]
                         }
                         # Add y values if not provided
                         if(!("y" %in% names(data))){
                           data$y <- rep(0.5, nrow(data))
                           data$group <- as.integer(rep(1, nrow(data)))
                         }

                         data
                       },
                       draw_panel = function(data, panel_scales, coord, xmin, xmax, na.rm = FALSE) {
                         ## Transform the data first
                         coords <- coord$transform(data, panel_scales)

                         ## Let's print out the structure of the 'coords' object
                         str(coords)

                         #scale_x_continuous(limits = c(coords$xmin, coords$xmax))


                         ## Construct a grid grob
                         pointsGrob(
                           x = coords$x,
                           y = coords$y,
                           pch = 1
                         )
                       })

geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                         position = "identity", na.rm = FALSE,
                         show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = geom_timeline_proto, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
