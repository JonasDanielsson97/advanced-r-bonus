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
  origin_data <-
    dplyr::case_when(
      nyc_origin == "ALL_NYC" ~ NYC_airports,
      .default =
        NYC_airports |>
        dplyr::filter(origin == nyc_origin))

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

    # outside-of-map airports
    ggplot2::geom_point(
      data = off_map_data,
      ggplot2::aes(x = plot_x,y = plot_y,color = mean_delay)) +

    ggplot2::geom_text(
      data = off_map_data,
      ggplot2::aes(x = plot_x, y = plot_y, label = dest),hjust = -0.3, size = 2.5) +

    ggplot2::annotate(
      "text", x = -125,y = 26, label = "Airports outside map:",
      hjust = 0, fontface = "italic", size = 3) +

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
