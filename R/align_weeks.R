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
ref_date_in_season <- function(season, season_start_month, season_start_day, ref_month, ref_day) {
  before_season_start <- (
    (ref_month < season_start_month) |
      ((ref_month == season_start_month) & (ref_day < season_start_day))
  )

  ISOdate(season + before_season_start, ref_month, ref_day)
}

#' Validate argument lengths
#'
#' All arguments must be of the same length, or they call all be of
#' length 1 except for a single argument
#'
#' @param ... named or positional arguments
#'
#' @return `TRUE`, or throw error
validate_lengths <- function(...) {
  # named list of the arguments
  args <- list(...)
  # number of arguments
  n_args <- length(args)
  # vector of lengths of each argument, potentially named
  lens <- sapply(args, length)
  # number of unique argument lengths
  n_unique_lens <- length(unique(lens))
  # number of arguments of length 1
  n_len_1 <- sum(lens == 1)
  # vector of number of times each argument-length occurs
  tab <- tabulate(lens)

  # otherwise, check that all arguments except one are of
  # length 1
  if (n_unique_lens == 1) {
    # if all arguments are the same length, pass!
    return(TRUE)
  } else if (n_unique_lens == 2 && n_args == n_len_1 + 1) {
    # if there are two unique lengths, and only one argument
    # is not of length 1, also pass!
    return(TRUE)
  } else {
    stop(paste0(c("Mismatched argument lengths:", lens, collapse = " ")))
  }
}
