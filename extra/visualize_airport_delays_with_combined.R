
#' Visualization of mean airport delays
#' This function uses the nycflights13 package
#' <https://github.com/tidyverse/nycflights13">
#' providing data on flight schedule deviations
#' for flights originating in one of the three New York City airports.
#' Foocusing on arrival delays, destination airport mean arrival delay times
#' are displayed on a color scale.
#' Note: Mean delay times are calculated with early arrivals set to 0,
#' since early arrivals are not mitigating actual delays
#' @returns A list of five plots: one map plot for arrival delays for each
#' of the NYC airports, one for them treated as one origin airport,
#' and one with the previous four plots combined.
#'
#' @export
visualize_airport_delays <- function(){

################################################################################
##### HELPER
################################################################################

##### joining in airport data with geometry added #####
add_airport_info <- function(faa_table,airports_table, faa_match = dest) {
  complete <-
    faa_table |>
    dplyr::left_join(airports_table, by = dplyr::join_by({{faa_match}} == faa)) |>
    # remove airports not present in airports_table
    dplyr::filter(!is.na(name)) |>
    # recreating a sf object (the sf attribute was lost in the left_join)
    sf::st_as_sf()
  return(complete)
}

################################################################################
##### DATA IMPORT AND PREPARATION
################################################################################

##### extract useful airport data #####
airports_data <-
  nycflights13::airports |>
  dplyr::select(faa, name, lat, lon) |>
  dplyr::arrange(lat, lon) |>
  # add Simple Features geometry
  # the points will be in a column named geometry and
  # the tibble will become an sf object
  # (st_ stands for Spatial and Temporal)
  sf::st_as_sf(
    coords = c("lon", "lat"),
    crs = 4326,
    remove = FALSE
  )

##### extract and useful airport data #####
flights_data<-
  nycflights13::flights |>
  dplyr::select(origin, dest, dep_delay, arr_delay) |>
  # remove missing data
  dplyr::filter(!(is.na(dep_delay) | is.na(arr_delay))) |>
  # create a long format separating departure and arrival delays
  tidyr::pivot_longer(
    cols = tidyr::ends_with("_delay"),
    names_to = "event",
    values_to = "delay"
  ) |>
  dplyr::mutate(event = dplyr::replace_values(event,
                 "dep_delay" ~ "departure",
                 "arr_delay" ~ "arrival")) |>
  dplyr::mutate(status =
                  dplyr::case_when(
                    (delay < 0) ~ "early",
                    (delay == 0) ~ "precise",
                    (delay > 0) ~ "delayed"))

##### compute means #####
arrival_delays <-
  flights_data |>
  dplyr::filter(event == "arrival") |>
  # set delay = 0 for early flights
  dplyr::mutate(delay = pmax(delay, 0))

mean_arrival_delays <-
  dplyr::bind_rows(
    # mean delays for each NYC origin airport
    arrival_delays |>
      dplyr::summarize(mean_delay = mean(delay),.by = c(origin, dest)),

    # mean delays for all NYC airports combined
    arrival_delays |>
      dplyr::summarize(mean_delay = mean(delay), .by = dest) |>
      dplyr::mutate(origin = "ALL_NYC")) |>

  # add destination airport information
  add_airport_info(airports_data, faa_match = dest) |>

  # flag airports outside the map
  dplyr::mutate(outside_map_box = lon < -130)


##### Origin = NYC airport info #####

NYC_airports <-
  dplyr::tibble(nyc = c("EWR", "LGA", "JFK")) |>
  add_airport_info(airports_data, faa_match = nyc) |>
  dplyr::mutate(origin = nyc)

################################################################################
##### VISUALIZE
################################################################################

##### prepare map #####

# compute the bounding box
# (outer limits for the coordinates in the table)
bbox <- sf::st_bbox(mean_arrival_delays |>
                      dplyr::filter(!outside_map_box))
# US boundaries
us_states <- USAboundaries::us_states()

delay_colors <- c("#FFFFBF", "#FEE090","#FDAE61", "#F46D43", "#D73027")

# fix map positions for out-of-map airports
off_map_mean_arrivals <-
  mean_arrival_delays |>
  sf::st_drop_geometry() |>
  dplyr::filter(outside_map_box) |>
  dplyr::mutate(plot_x = -121, plot_y = 25-1.5*dplyr::row_number())

off_map_mean_arrivals <-
  mean_arrival_delays |>
  sf::st_drop_geometry() |>
  dplyr::filter(outside_map_box) |>
  dplyr::arrange(origin, dest) |>
  dplyr::mutate(
    plot_x = -121,
    plot_y = 25 - 1.5 * dplyr::row_number(),
    .by = origin
  )

##### plot on map #####

plot_map <- function(nyc_origin) {

  # choose nyc origin
  plot_data <-
    mean_arrival_delays |>
    dplyr::filter(origin == nyc_origin)

  # off-map airports
  off_map_data <-
    off_map_mean_arrivals |>
    dplyr::filter(origin == nyc_origin)

  # origin airport markers
  origin_data <- NYC_airports
  if (nyc_origin == "ALL_NYC") {
    origin_data <-
      origin_data |>
      dplyr::filter(origin == nyc_origin)
  }

  # create map
  gp <-
    ggplot2::ggplot() +

    # US map
    ggplot2::geom_sf(data = us_states,fill = "grey95",color = "grey70") +

    # origin airport points
    ggplot2::geom_sf(
      data = origin_data,shape = 6,color = "black",size = 2) +

    # destination airports with mean delay > 0
    ggplot2::geom_sf(
      data = plot_data |> dplyr::filter( mean_delay > 0,!outside_map_box),
      ggplot2::aes(color = mean_delay)) +

    # same color scal for all origins
    ggplot2::scale_color_gradientn(colors = delay_colors,
                                   limits = c(0,max(mean_arrival_delays$mean_delay))) +

    # destination airports with mean delay == 0
    ggplot2::geom_sf(
      data = plot_data |> dplyr::filter(mean_delay == 0,!outside_map_box),
      color = "springgreen3") +

    # map boundaries
    ggplot2::coord_sf(
      xlim = c(bbox["xmin"] - 5,bbox["xmax"] + 5), ylim = c(bbox["ymin"] - 5,bbox["ymax"] + 5),
      expand = FALSE, default_crs = sf::st_crs(4326)) +

    # outside-of-map airports if any
    (if (nrow(off_map_data !=0)) {
      list(
        ggplot2::geom_point(
      data = off_map_data,
      ggplot2::aes(x = plot_x,y = plot_y,color = mean_delay)),

      ggplot2::geom_text(
        data = off_map_data,
        ggplot2::aes(x = plot_x, y = plot_y, label = dest),hjust = -0.3, size = 2.5),

      ggplot2::annotate(
        "text", x = -125,y = 26, label = "Airports outside map:",
        hjust = 0, fontface = "italic", size = 3))
    })+

    # theme settings and labs
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "right",
      axis.title.y = ggplot2::element_text(size = 8),
      axis.title.x = ggplot2::element_text(size = 8),
      legend.text = ggplot2::element_text(size = 8),
      legend.title = ggplot2::element_text(size = 8),
      plot.caption = ggplot2::element_text(face = "italic",hjust = 0,size = 8,colour = "grey40")) +

    ggplot2::labs(
      title = paste("Mean arrival delays -", nyc_origin), x = NULL,y = NULL, color = "Delay (minutes)")

  return(gp)
}

##### create individual plots #####

plots <-
  list(EWR = plot_map("EWR"),JFK = plot_map("JFK"), LGA = plot_map("LGA"), ALL_NYC = plot_map("ALL_NYC"))

##### create combined patchwork plot #####

combined <-
  (plots$EWR | plots$JFK) /
  (plots$LGA | plots$ALL_NYC)

# legends and shared caption
combined <-
  combined +
  patchwork::plot_layout(guides = "collect") +
  patchwork::plot_annotation(
    title = "Mean arrival delays for NYC-originated flights, 2013",
    caption = paste(
      "Destination airports are colored by mean arrival delay.",
      "Early arrivals are treated as zero delay.",
      "HNL and ANC are shown separately.",
      "Origin airports are marked with black triangles.",
      "\nData: https://github.com/tidyverse/nycflights13"
    )
  ) &
  ggplot2::theme(legend.position = "right")

##### return all five plots #####

plots$combined <- combined

return(plots)
}

# ##### plot on map #####
# gp <-
#   ggplot2::ggplot() +
#   # first plot the US map
#   ggplot2::geom_sf(data = us_states, fill = "grey95", color = "grey70") +
#
#   # plot origin airport points
#   ggplot2::geom_sf(data = NYC_airports, shape = 6, color = "black", fill = "black", size = 2) +
#
#   # plot destination airport points with delay > 0
#   ggplot2::geom_sf(data = mean_arrival_delays |>
#                      dplyr::filter((mean_delay>0) & (!outside_map_box)),
#           ggplot2::aes(color = mean_delay)) +
#   ggplot2::scale_color_gradientn(colours = delay_colors) +
#
#   # plot destination airport with mean delay == 0
#   ggplot2::geom_sf(data = mean_arrival_delays |> dplyr::filter(mean_delay==0),color = "springgreen3") +
#   ggplot2::coord_sf(
#     xlim = c(bbox["xmin"] - 5, bbox["xmax"] + 5),
#     ylim = c(bbox["ymin"] - 5, bbox["ymax"] + 5),
#     expand = FALSE
#   ) +
#
#   # plot outside-of-map airports
#   ggplot2::geom_point(data = off_map_mean_arrivals,
#     aes(x = plot_x, y = plot_y, color = mean_delay)
#   ) +
#   ggplot2::geom_text(data = off_map_mean_arrivals,
#     ggplot2::aes(x = plot_x, y = plot_y,
#         label = dest),
#     hjust = -0.3,
#     size = 2.5
#   ) +
#   ggplot2::annotate("text", x = -125, y = 26, label = "Airports outside map:",
#     hjust = 0, fontface = "italic", size = 3
#   )+
#   ggplot2::theme_minimal() +
#   ggplot2::facet_wrap(~ origin, nrow = 3) +
#   ggplot2::theme(legend.position = "right",
#         axis.title.y = ggplot2::element_text(size=8),
#         axis.title.x = ggplot2::element_text(size=8),
#         legend.text = ggplot2::element_text(size =8),
#         legend.title = ggplot2::element_text(size =8),
#         plot.caption = ggplot2::element_text(face = "italic",
#                                     hjust = 0,size = 8,colour = "grey40")) +
#   ggplot2::labs(title =
#          expression("Mean " * bold("arrival") * " delays for NYC-originated flights 2013"),
#        x = NULL, y = NULL,
#        color = "Delay (minutes)",
#        caption = paste("\nDestination airports (in the United States, Puerto Rico and the American Virgin Islands)",
#        "\nfor flights originating in New York City 2013.",
#        "on a color scale according to mean arrival delay.",
#        "\nMarkers for airports in Honolulu (HNL) and Alaska (ANC) are shown separately. ",
#        "\nAirport with no delay is marked green.",
#        "\nOrigin airports (JFK,LGA,EWR) are marked with black triangles.",
#        "\nData: https://github.com/tidyverse/nycflights13"))
#
# return(print(gp))
# }
