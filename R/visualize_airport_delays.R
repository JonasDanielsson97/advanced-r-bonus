library(dplyr)
library(nycflights13)
library(ggplot2)


NYC_airports <- c("EWR", "LGA", "JFK")

visualize_airport_delays <- function(){

}

##### extract useful airport data #####

airports_data <-
  nycflights13::airports |>
  select(faa, name, lat, lon) |>
  arrange(lat, lon)

##### extract useful airport data #####

flights_data <-
  nycflights13::flights |>
  select(origin, dest, dep_delay, arr_delay)

##### join airports and flights data

mean_delays <-
  flights_data |>
  dplyr::rename(
    airport_origin = origin,
    airport_dest = dest,
    delay_origin = dep_delay,
    delay_dest = arr_delay
  ) |>
  tidyr::pivot_longer(
    cols = c(starts_with("airport_"), starts_with("delay_")),
    names_to = c(".value", "direction"),
    names_sep = "_"
  ) |>
  filter(!is.na(delay)) |>
  summarize(mean_delay = mean(delay), .by = c(airport, direction)) |>
  arrange(airport)


mean_delays |>
  filter(direction == "origin") |>
  ggplot() +
  aes(x=airport, y = mean_delay)+
  geom_point()


mean_delays |>
  filter(direction == "dest") |>
  arrange(mean_delay) |>
  mutate(airport = forcats::fct_reorder(airport, mean_delay)) |>
  ggplot() +
  aes(x=airport, y = mean_delay)+
  geom_point()


