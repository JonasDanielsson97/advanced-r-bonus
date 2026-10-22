################################################################################
# DATA RETRIEVAL
################################################################################

#' Get all zones data
#' Stores all zones data in R_user_dir.
#' Read chached data if it exists, updates only if needed ,
#' and not again within less than 32 from previous succesful download.
#'
#' @param max_age_days Numerical value, user preference for maximum age of cached data to be used.
#' (Is adjusted to be at least 32 minutes)
#' @param force_update If TRUE; Update data if the turfgame API just allows it. Defaults to FALSE.
#' @param allow_stale If TRUE: Allow use of outdated data if renewal fails. Defaults to TRUE.
#'
#' @returns A tibble with all zones data
#' @export
turf_get_all_zones_data <- function(max_age_days = 30,
                                    force_update = FALSE,
                                    allow_stale = TRUE) {
  # ensure max_age_days is at least 32 minutes...
  min_age_days <- 32 / (24 * 60)
  if (max_age_days < min_age_days) {
    message("Minimum max_age_days is 32 minutes.")
    max_age_days <- min_age_days
  }

  # set file path
  zones_all_cached_file <- file.path(
    tools::R_user_dir("732A94_F_lab05", "data"),
    "732A94_group_F_lab05_zones_all.rds")

  cached_data_exists <- file.exists(zones_all_cached_file)

  # Check for cached data
  if (cached_data_exists) {
    # Read cached data
    zones_all <- readRDS(zones_all_cached_file)

    # Calculate and check age of stored data
    age <- difftime(Sys.time(), zones_all$time_of_retrieval[1])

    age_days <- as.numeric(age, units = "days")
    age_mins <- as.numeric(age, units = "mins")

    # 30-minute API restriction plus safety margin
    too_recent <- age_mins < 32

    # update if forced or outdated, but respect API limit
    API_data_needed <-
      (force_update | (age_days > max_age_days)) & !too_recent

    # Forced update requested but API limit prevents it
    if (force_update && too_recent) {
      message("Update skipped: Turf data was retrieved ",
              "less than 32 minutes ago.")
    }

  } else {
    # No previous all-zones data file found
    zones_all <- NULL
    API_data_needed <- TRUE
  }

  # Retrieve fresh data if needed
  if (API_data_needed) {
    message("Fetching all zones data.\n",
            "This may take a while, please be patient.")

    updated_zones_data <- turf_zones_all()

    if (!is.null(updated_zones_data)) {
      # Successful update: save new data
      dir.create(
        dirname(zones_all_cached_file),
        recursive = TRUE,
        showWarnings = FALSE)

      saveRDS(updated_zones_data, zones_all_cached_file)

      zones_all <- updated_zones_data

    } else {
      # Update failed: use cached data if permitted
      if (cached_data_exists && allow_stale) {
        warning(
          "Using outdated Turf data retrieved on ",
          format(zones_all$time_of_retrieval[1], "%Y-%m-%d %H:%M"),
          call. = FALSE)

      } else {
        stop("No usable Turf data available.", call. = FALSE)
      }
    }
  }

  # Return fresh or permitted cached data
  return(zones_all)
}

##### STATISTICS ##################################################################

##### GENERAL STATISTICS #####
#' Get current game statistics
#'
#' @return A list with statistics such as `totalUsers` and `usersOnline`.
#' @export
#' @examples
#' \dontrun{
#' turf_statistics()
#' }
turf_statistics <- function() {
  turf_get("statistics")
}

##### REGIONS ##################################################################

##### ALL REGIONS #####
#' Get all regions
#'
#' @return A data frame with one row per region.
#' @export
#' @examples
#' \dontrun{
#' turf_regions()
#' }
turf_regions <- function() {
  turf_get("regions")
}

##### ZONES ####################################################################

##### ZONES BY NAME #####
#' Get zones by zones
#' @param names A character vector of zone names.
#' @return A data frame with one row per zone that was found.
#' @export
#' @examples
#' \dontrun{
#' turf_zones(c("Kårallen", "Östergötland"))
#' }
turf_zones <- function(names) {
  stopifnot(is.character(names), length(names) > 0)

  # makes names into a list of lists
  body <- lapply(names, function(name) list(name = name))
  turf_post("zones", body)
}

##### USERS ####################################################################

##### USERS BY NAME #####
#' Get users by name
#'
#' @param names A character vector of user names.
#' @param warn If `TRUE` (default), give a warning listing the names that
#'   were not found. Set to `FALSE` to suppress it.
#' @return A data frame with one row per user that was found, or an empty
#'   list if none were found.
#' @export
#' @examples
#' \dontrun{
#' turf_users(c("fredrick", "ingrid"))
#' turf_users("not_a_player", warn = FALSE)
#' }
turf_users <- function(names, warn = TRUE) {
  stopifnot(is.character(names), length(names) > 0)
  stopifnot(is.logical(warn), length(warn) == 1, !is.na(warn))

  # Convert string vector to one list per name, returned as list.
  # makes names into a list of lists basicly
  body <- lapply(names, function(name) list(name = name))
  users <- turf_post("users", body)

  # The API silently drops unknown names and matches case-insensitively
  missing <- names[!tolower(names) %in% tolower(users$name)]
  if (warn && length(missing) > 0) {
    warning("User(s) not found: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  users
}

##### USERS TOPLIST #####
#' Get the top list of users
#'
#' @param from First place to include.
#' @param to Last place to include.
#' @param country Optional country code, e.g. `"se"`.
#' @param region Optional region name, e.g. `"kalmar"`.
#' @return A data frame with one row per user, ordered by place.
#' @export
#' @examples
#' \dontrun{
#' turf_top(1, 10)
#' turf_top(1, 10, country = "se")
#' }
turf_top <- function(from = 1, to = 50, country = NULL, region = NULL) {
  stopifnot(is.numeric(from), is.numeric(to), from <= to)

  body <- list(from = from, to = to)
  body$country <- country
  body$region <- region
  turf_post("users/top", body)
}

################################################################################
# PLAYERS
################################################################################

##### GET (CURRENTLY) ACTIVE PLAYERS #####
#' Get (currently) active players
#'
#' @returns A tibble with zone data with polygonal locations and retrieval time stamp
#' @export
all_active_players <- function(){
  turf_get("users/location") |>
    dplyr::as_tibble() |>
    # adding time of data retrieval
    dplyr::mutate(time_of_request = Sys.time()) |>
    # adding vector geometry
    sf::st_as_sf(
      coords = c("longitude", "latitude"),
      crs = 4326,
      remove = FALSE
    ) |>
    dplyr::select(name, id, latitude, longitude, geometry)
}


################################################################################
# MAP
################################################################################

##### DISPLAY ON MAP #####
#' Display zones and active players, in an area of choice, on a map
#'
#' @param zones_all A tibble with info on all turfgame zones as created by turf_allzones_data()
#' @param address A character string or vector (e.g, city, village, street)
#' @param turfarea_radius A numerical values, radius (in kilometers) of area to cover
#' @param show_zones TRUE (shows) or FALSE (does not show) zones on the map
#' @param show_players TRUE (shows) or FALSE (does not show) active players on the map
#'
#' @returns Displays zones and active players
#' @export
#'
display_zones_and_active_players <- function(zones_all,
                                             address,
                                             turfarea_radius = 20,
                                             show_zones = TRUE,
                                             show_players = TRUE){

  turfarea_pos <- get_geo_pos(address)

  turfarea_koord <-
    turfarea_pos |>
    dplyr::select(lat, long) |>
    unlist()

  turfarea_center <-
    get_sf_point_from_koord(turfarea_koord)

  turfarea_zones <-
    turfarea_center|>
    zones_around(turfarea_radius, zones_all)

  turfarea_active_players <-
    all_active_players() |>
    active_players_in_area(turfarea_center, dist = turfarea_radius*1000)

  show_zones <- TRUE
  show_players <- TRUE

  map_positions(turfarea_zones,
                turfarea_active_players,
                show_zones = show_zones,
                show_players = (show_players &!is.null(turfarea_active_players)),
                show_legend = TRUE)


}

