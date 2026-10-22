##### MAP ######################################################################

library(leaflet)
library(sf)

##### GEOMETRIC POINT #####
#' Get a geometric point from latitude and longitude coordinates
#' (from our lab05 package)
#' @param lat_long_vec Numerical vector of size 1x2, latitude in pos 1, longitude in pos 2
#'
#' @returns sfc class object
#'
get_sf_point_from_koord <- function(lat_long_vec){
  sf::st_sfc(
    sf::st_point(c(lat_long_vec[2], lat_long_vec[1])), # long, lat
    crs = 4326
  )
}



#' map positions of airports using leaflet:: package
#'
#' @param area_data sf A table with zones
#' @param players sf A table with players, defaults to NULL
#' @param show_zones TRUE or FALSE defaults to TRUE
#' @param show_players TRUE or FALSE, defult depending on players being NULL or not
#' @param show_legend TRUE or FALSE, defaults to TRUE
#'
#' @returns prints map with optional players and zones
# map_positions <- function(area_data,
#                           players = NULL,
#                           show_zones = TRUE,
#                           show_players = !is.null(players),
#                           show_legend = TRUE) {
  delay_colors <- c("#4575B4","#74ADD1","#ABD9E9","#E0F3F8","#FFFFBF","#FEE090","#FDAE61","#F46D43","#D73027"
  )
  
  pal <- 
    leaflet::colorNumeric(
    palette = delay__colors,
    domain = mydata$value,
    na.color = "transparent"
  )
  

# create a map 
the_map <-
    leaflet::leaflet() |>
    leaflet::addTiles()
  
  # add zones
  if (show_zones) {
    the_map <- the_map |>
      leaflet::addCircleMarkers(
        data = area_data,
        radius = 5,
        stroke = FALSE,
        fillColor = ~ pal(pointsPerHour),
        fillOpacity = 0.8,
        popup = ~ paste("<b>", name, "</b>", "<br>PPH:", pointsPerHour))
  }
  
  # add players
  if (show_players) {
    if (show_players) {
      the_map <- the_map |>
        leaflet::addAwesomeMarkers(
          data = players,
          icon = leaflet::makeAwesomeIcon(
            text = "P",
            markerColor = "blue",
            iconColor = "white"
          ),
          popup = ~paste0(
            "<b>", htmltools::htmlEscape(name), "</b>"
          )
        )
    }
  }
  
  # add legend
  if (show_zones && show_legend) {
    the_map <-
      the_map |>
      leaflet::addLegend(pal = pal,
                         values = 1:9,
                         title = "Points per hour")
  }
  
  return(the_map)
}



##### original map
map_positions <- function(area_data,
                          players = NULL,
                          show_zones = TRUE,
                          show_players = !is.null(players),
                          show_legend = TRUE) {
  pph_colors <- c(
    "1" = "#4575B4",
    "2" = "#74ADD1",
    "3" = "#ABD9E9",
    "4" = "#E0F3F8",
    "5" = "#FFFFBF",
    "6" = "#FEE090",
    "7" = "#FDAE61",
    "8" = "#F46D43",
    "9" = "#D73027"
  )
  
  pal <- leaflet::colorFactor(palette = pph_colors, domain = 1:9)
  
  m <-
    leaflet::leaflet() |>
    leaflet::addTiles()
  
  # add zones
  if (show_zones) {
    m <- m |>
      leaflet::addCircleMarkers(
        data = area_data,
        radius = 5,
        stroke = FALSE,
        fillColor = ~ pal(pointsPerHour),
        fillOpacity = 0.8,
        popup = ~ paste("<b>", name, "</b>", "<br>PPH:", pointsPerHour))
  }
  
  # add players
  if (show_players) {
    if (show_players) {
      m <- m |>
        leaflet::addAwesomeMarkers(
          data = players,
          icon = leaflet::makeAwesomeIcon(
            text = "P",
            markerColor = "blue",
            iconColor = "white"
          ),
          popup = ~paste0(
            "<b>", htmltools::htmlEscape(name), "</b>"
          )
        )
    }
  }
  
  # add legend
  if (show_zones && show_legend) {
    m <-
      m |>
      leaflet::addLegend(pal = pal,
                         values = 1:9,
                         title = "Points per hour")
  }
  
  return(m)
}

