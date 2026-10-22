################################################################################
# HIDDEN (not exported): REQUESTS
################################################################################

# Internal (HIDDEN) helpers that are ment to talk to the Turf API.
turf_base_url <- "https://api.turfgame.com/v5" # Start of URL

# Creates request object
## Endpoint is the end of URL like: "statistics" or "regions"
turf_request <- function(endpoint) {
  httr2::request(turf_base_url) |>            # Base URL
    httr2::req_url_path_append(endpoint) |>   # Adds endpoint
    httr2::req_throttle(rate = 1) |>          # Limit to 1 request per second
    httr2::req_error(body = function(resp) {  # Show API error message
      httr2::resp_body_json(resp)$errorMessage
    })
}

# Send a GET request and return the parsed JSON.
turf_get <- function(endpoint) {
  turf_request(endpoint) |>                       # Prepare request with turf_request()
    httr2::req_perform() |>                       # Send request (GET)
    httr2::resp_body_json(simplifyVector = TRUE)  # JSON to R objects
}

# Send a POST request with `body` as JSON and return the parsed JSON.
turf_post <- function(endpoint, body) {
  turf_request(endpoint) |>                       # Prepare request with turf_request()
    httr2::req_body_json(body) |>                 # Convert (body) from R object to JSON
    httr2::req_perform() |>                       # Send request (POST)
    httr2::resp_body_json(simplifyVector = TRUE)  # JSON to R objects
}


################################################################################
# HIDDEN (not exported): DATA STORAGE
################################################################################

#' Send a request for all ones data and return result as ta tibble
#' NOTE: retrieval of all zones data is allowed by the turfgame API MAX ONCE PER 30 MINUTES
#'
#' @returns A tibble with information on all turfgame zones,
#' with added polygonal locations calculated using sf:: package
#'
turf_zones_all <- function() {
  z_all <- tryCatch({
    turf_get("zones/all") |>
      dplyr::as_tibble() |>
      # Add time of data retrieval
      dplyr::mutate(time_of_retrieval = Sys.time()) |>
      # Add vector geometry
      sf::st_as_sf(
        coords = c("longitude", "latitude"),
        crs = 4326,
        remove = FALSE
      )

  }, error = function(e) {
    warning("Turf API update failed: ", conditionMessage(e), call. = FALSE)
    NULL
  })

  return(z_all)
}


################################################################################
# HIDDEN (not exported): MAP DISPLAY HELPERS
################################################################################

##### GEOMETRY #################################################################

##### POINT #####
#' Get a geometric point from koordinates
#'
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

##### ACTIVE PLAYERS ###########################################################
#' Extract active players within defined area
#'
#' @param active_players A tibble as created by all_active_players()
#' @param centerpoint A polygonal location point calculated by sf::st_sfc)
#' @param dist A numerical value for distance (meters) fo cover from centerpoint
#'
#' @returns A tibble (selection from active_players argument)
#'
active_players_in_area <-  function(active_players, centerpoint, dist=20000){
  active_players[
    sf::st_is_within_distance(active_players,
                              centerpoint,
                              dist = dist,
                              sparse = FALSE)[,1],
  ]
}

##### ADDRESSES #################################################################

##### GET GEO POSITION #####
#' Helper to get_geo_pos()
#'
#' @param address A character string or character vector
#'
#' @returns A tibble with geocode data for the address
get_geo_tibble <- function(address){
  geo_tibble <-
    tidygeocoder::geo(
      address = address,
      method = "osm",
      limit = 10,
      full_results = TRUE,
      custom_query = list(addressdetails = 1))

  return(geo_tibble)
}

##### GET LATITUDE AND LONGITUDE #####
#' Get latitude and longitude for an address
#'
#' @param address An address haracter string or vector
#' @param prio_addresstype Optional string for prioritized address type
#'
#' @returns A tibble with the name of the address,
#' and its latitude and longitude
get_geo_pos <- function(address, prio_addresstype = NA_character_) {
  prio <- c("city", "town", "village", "hamlet", "road")

  stopifnot("address must be a character string or vector"
            = is.character(address),
            "If prioritized address type is give, it must be one of: city, town, village, hamlet, or road"
            = (is.na(prio_addresstype) | (prio_addresstype %in% prio)))

  # prioritize location types
  prio <- c("city", "town", "village", "hamlet", "road")
  if (!is.na(prio_addresstype)) {
    # forcing to a specific type of location
    prio <- prio_addresstype
  }

  geo_pos <-
    get_geo_tibble(address) |>
    dplyr::filter(addresstype %in% prio) |>
    dplyr::mutate(prio = match(addresstype, prio)) |>
    dplyr::group_by(address) |>
    dplyr::slice_min(prio, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(name, osm_address.country, lat, long) |>
    dplyr::rename(country = osm_address.country)

  return(geo_pos)
}

##### ZONES ####################################################################

#' Gets zones within a given radius distance from
#' a specified point
#'
#' @param center A sf point for center point of area in focus
#' @param zones A tibble with all zones data the center point
#' @param km A number, radius distance to cover in kilometers, defaults to 20
#'
#' @returns A sf table
#'
zones_around <- function(center, km = 20, zones) {

  zones[
    sf::st_is_within_distance(
      zones,
      center,
      dist = km * 1000,
      sparse = FALSE
    )[, 1],
  ]
}

##### MAP ######################################################################

#' map positions of zones and players using leaflet:: package
#'
#' @param area_data sf A table with zones
#' @param players sf A table with players, defaults to NULL
#' @param show_zones TRUE or FALSE defaults to TRUE
#' @param show_players TRUE or FALSE, defult depending on players being NULL or not
#' @param show_legend TRUE or FALSE, defaults to TRUE
#'
#' @returns prints map with optional players and zones
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
