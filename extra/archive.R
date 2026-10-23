# mean_delays <-
#   flights_data |>
#   # create a long format separating departure and arrival delays
#   dplyr::rename(
#     airport_origin = origin,
#     airport_dest = dest,
#     delay_origin = dep_delay,
#     delay_dest = arr_delay
#   ) |>
#   tidyr::pivot_longer(
#     cols = c(starts_with("airport_"), starts_with("delay_")),
#     names_to = c(".value", "direction"),
#     names_sep = "_"
#   ) |>
#   # remove missing data
#   filter(!is.na(delay)) |>
#   # is the flight on time or delayed?
#   mutate(isdelayed = delay >=0) |>
#   # keep only on time or delayed
#   # ( = skip flights leaving ahead of schedule)
#   filter(isdelayed) |>
#   # compute mean delays, per airport and departure /arrival
#   summarize(mean_delay = mean(delay), .by = c(airport, direction)) |>
#   arrange(airport)


##### arranging palette #####
delay_colors <- c("#4575B4","#74ADD1","#ABD9E9","#E0F3F8","#FFFFBF","#FEE090","#FDAE61","#F46D43","#D73027"
)

pal <-
  leaflet::colorNumeric(
    palette = delay__colors,
    domain = mydata$value,
    na.color = "transparent"
  )
# world multipolygon
world <-
  rnaturalearth::ne_countries(
    scale = "medium",
    returnclass = "sf"
  )
# # background world map
# geom_sf(
#   data = us_states,
#   fill = "grey95",
#   color = "grey70",
#   linewidth = 0.3
# ) +

# high resolution data in the rnaturalearthhires package
us_states <-
  rnaturalearth::ne_states(
    country = "united states of america",
    returnclass = "sf"
  )
################################################################################




flight_delays <-
  flights_precision |>
  # keep only exactly on time or delayed
  # ( = skip flights leaving ahead of schedule)
  dplyr::filter(status %in% c( "precise","delayed"))


mean_delays_by_nyc_airport <-
  flight_delays |>
  # compute mean delays, per status, origin, airport and departure /arrival
  dplyr::summarize(mean_delay = mean(delay), .by = c(status, origin, dest, event)) |>
  # add names of destination airports (+ excluding unlisted airports)
  add_airport_info(airports_data, faa_match = dest)

mean_arrival_delays_nyc_total_by_destination <-
  mean_delays_nyc_total |>
  dplyr::filter(event == "arrival")

mean_departure_delays_nyc_total_by_destination <-
  mean_delays_nyc_total |>
  dplyr::filter(event == "departure")

mean_arrival_delays_by_nyc_airport_and_destination <-
  mean_delays_by_nyc_airport |>
  dplyr::filter(event == "arrival")

mean_departure_delays_by_nyc_airport_and_destination <-
  mean_delays_by_nyc_airport |>
  dplyr::filter(event == "departure")


delays_table <- mean_arrival_delays_nyc_total_by_destination

# simple plot
delays_table |>
  ggplot2::ggplot()+
  ggplot2::aes(x=lon, y=lat, color = mean_delay)+
  ggplot2::geom_point()+
  ggplot2::scale_color_gradientn(
    colours = c(
      "#E0F3F8", "#FFFFBF", "#FEE090",
      "#FDAE61", "#F46D43", "#D73027"
    ))+
  theme_minimal()+
  labs(title = "Mean arrival delays for NYC-originated flights 2013",
       x = "Airport longitude (°)", y = "Airport latitude (°)",
       color = "Mean arrival delay")

# plot with part of world map
delays_table |>
  ggplot2::ggplot()+
  # first plot the map
  ggplot2::geom_sf(data = world,fill = "grey95",color = "grey70",linewidth = 0.3) +
  ggplot2::geom_point(aes(x=lon, y=lat, color = mean_delay))+
  ggplot2::scale_color_gradientn(
    colours = c(
      "#E0F3F8", "#FFFFBF", "#FEE090",
      "#FDAE61", "#F46D43", "#D73027"
    ))+
  theme_minimal()+
  coord_sf(
    xlim = range(delays_table$lon, na.rm = TRUE) + c(-5, 5),
    ylim = range(delays_table$lat, na.rm = TRUE) + c(-5, 5),
    expand = FALSE
  ) +
  labs(title = "Mean arrival delays for NYC-originated flights 2013",
       x = "Airport longitude (°)", y = "Airport latitude (°)",
       color = "Mean arrival delay")
