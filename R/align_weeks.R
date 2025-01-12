align_weeks <- function(
    x,
    target_season,
    ref_month = 1,
    ref_day = 1,
    season_start_month = 7,
    season_start_day = 1,
    week_day = 6) {
  # assign each input date to a season
  seasons <- get_season(
    x,
    start_month = season_start_month,
    start_day = season_start_day
  )

  # each season has a reference date
  ref_dates <- get_ref_date(
    seasons,
    start_day = season_start_day, start_month = season_start_month
  )
  target_ref_date <- get_ref_date(
    target_season,
    start_day = season_start_day,
    start_month = season_start_month
  )

  # each season's reference date corresponds to a reference week date
  ref_week_dates <- lubridate::ceiling_date(
    ref_dates,
    unit = "weeks", week_start = week_day
  )
  target_ref_week_date <- lubridate::ceiling_date(
    target_ref_date,
    unit = "weeks", week_start = week_day
  )

  # optimal number of weeks to offset each season
  offset_days <- (target_ref_week_date - ref_week_dates) / lubridate::ddays(1)
  offset_weeks <- round(offset_days / 7)

  # move each input date by the offset weeks
  x + lubridate::weeks(offset_weeks)
}

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
  before_season_start <- (
    lubridate::month(x) < start_month |
      ((lubridate::month(x) == start_month) & (lubridate::day(x) < start_day))
  )
  # if before season start, then you belong to the season
  # starting in the previous calendar year. this logic
  # relies on casting FALSE->0 and TRUE->1
  lubridate::year(x) - before_season_start
}

get_ref_date <- function(season, start_day, start_month) {

}
