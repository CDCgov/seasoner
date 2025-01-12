#' Get the two-year season belong to a particular date
#'
#' Seasons are 1-year (i.e., 365 or 366) days periods beginning on some
#' month and day. Typically, seasons are denoted by two years. E.g.,
#' January 1, 2020 is in the 2019/2020 season.
#'
#' In this codebase, a "season" is encoded by a single number, which is
#' the first year of the two-year season.
#'
#' @param x dates
#' @param start_month integer month of the season start (1=January)
#' @param start_day integer day of the season start
#'
#' @return integer seasons (indicating first year of two-year season)
date_to_season <- function(x, start_month, start_day) {
  validate_lengths(x, start_month, start_day)

  before_season_start <- (
    (lubridate::month(x) < start_month) |
      ((lubridate::month(x) == start_month) & (lubridate::day(x) < start_day))
  )
  # if before season start, then you belong to the season
  # starting in the previous calendar year. this logic
  # relies on casting FALSE->0 and TRUE->1
  lubridate::year(x) - before_season_start
}

#' Get the reference date in a season
#'
#' @param season First year of two-year season
#' @param season_start_month Integer month of season start (1=January)
#' @param season_start_day Calendar day of season start
#' @param ref_month Integer month of reference dates
#' @param ref_day Calendar day of reference dates
#'
#' @return vector of dates
ref_date_in_season <- function(
    season, season_start_month, season_start_day, ref_month, ref_day) {
  validate_lengths(
    season, season_start_month, season_start_day, ref_month, ref_day
  )

  before_season_start <- (
    (ref_month < season_start_month) |
      ((ref_month == season_start_month) & (ref_day < season_start_day))
  )



  ISOdate(season + before_season_start, ref_month, ref_day)
}
