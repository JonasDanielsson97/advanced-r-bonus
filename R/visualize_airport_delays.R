library(dplyr)
library(nycflights13)
library(ggplot2)
library(rnaturalearth) # also needs the rnaturalearthdata package to be installed
library(USAboundaries)
library(sf)

################################################################################
##### HELPER
################################################################################

##### joining in airport data with geometry added #####
add_airport_info <- function(faa_table,airports_table, faa_match = dest) {
  complete <-
    faa_table |>
    left_join(airports_table, by = join_by({{faa_match}} == faa)) |>
    # remove airports not present in airports_table
    filter(!is.na(name)) |>
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
  select(faa, name, lat, lon) |>
  arrange(lat, lon) |>
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
  select(origin, dest, dep_delay, arr_delay) |>
  # remove missing data
  dplyr::filter(!(is.na(dep_delay) | is.na(arr_delay))) |>
  # create a long format separating departure and arrival delays
  tidyr::pivot_longer(
    cols = ends_with("_delay"),
    names_to = "event",
    values_to = "delay"
  ) |>
  dplyr::mutate(event = dplyr::replace_values(event,
                 "dep_delay" ~ "departure",
                 "arr_delay" ~ "arrival")) |>
  dplyr::mutate(status =
           case_when(
             (delay < 0) ~ "early",
             (delay == 0) ~ "precise",
             (delay > 0) ~ "delayed"))

##### compute means #####
mean_arrival_delays <-
  flights_data|>
  filter(event == "arrival") |>
  # set delay = 0 for early flights
  dplyr::mutate(delay =
                  dplyr::replace_when(delay, status == "early" ~ 0)) |>
  # compute mean delays, per status, airport and departure /arrival
  dplyr::summarize(mean_delay = mean(delay), .by = c(status,dest, event)) |>
  # add names of destination airports (+ exclude unlisted airports)
  add_airport_info(airports_data, faa_match = dest)

##### Origin = NYC airport info #####
NYC_airports <-
  tibble(nyc =c("EWR", "LGA", "JFK"))|>
  add_airport_info(airports_data, faa_match = nyc)

################################################################################
##### VISUALIZE
################################################################################

##### prepare map #####

# compute the bounding box
# (outer limints for the coordinates in the table)
bbox <- st_bbox(delays_table)
# US boundaries
us_states <- USAboundaries::us_states()

# create the plot-on-map
ggplot() +
  # first plot the US map
  geom_sf(data = us_states, fill = "grey95", color = "grey70") +
  # plot origin airport points
  geom_sf(data = NYC_airports,color = "#4575B4") +
  # plot destination airport with mean delay == 0
  geom_sf(data = mean_arrival_delays |> filter(mean_delay>0),aes(color = mean_delay)) +
  scale_color_gradientn(
    colours = c("#FFFFBF", "#FEE090","#FDAE61", "#F46D43", "#D73027")
  ) +
  geom_sf(data = mean_arrival_delays |> filter(mean_delay==0),color = "#008000") +
  # plot destination airport points with delay > 0
  coord_sf(
    xlim = c(bbox["xmin"] - 5, bbox["xmax"] + 5),
    ylim = c(bbox["ymin"] - 5, bbox["ymax"] + 5),
    expand = FALSE
  ) +
  theme_minimal() +
  theme(legend.position = "top",
        plot.caption = element_text(face = "italic",
                                    hjust = 0,size = 8,colour = "grey40")) +
  labs(title = "Mean arrival delays for NYC-originated flights 2013",
       x = "Airport longitude (°)", y = "Airport latitude (°)",
       color = "Mean arrival delay (minutes)",
       caption = paste("\nDestination airports (in the United States, Puerto Rico,",
       "\nand the American Virgin Islands)",
       "for flights originating in New York City 2013.",
       "\non a color scale according to mean arrival delay.",
       "\nOrigin airports (JFK,LGA,EWR) in blue.",
       "\nData: https://github.com/tidyverse/nycflights13"))










visualize_airport_delays <- function(){



}


