#' Align weeks across seasons
#'
#' @param week_day integer day of the week to align to (1=Monday)
#'
#' @return vector of dates, of same length as `x`
#'
#' @export
align_weeks <- function(
    x,
    target_season,
    ref_month = 1,
    ref_day = 1,
    season_start_month = 7,
    season_start_day = 1,
    week_day = 6) {
  validate_lengths(
    x, target_season, ref_month, ref_day, season_start_month, season_start_day
  )

  # check that input dates are on teh week day in question
  stopifnot(rlang::is_integerish(week_day, n = 1))
  if (!all(lubridate::wday(x, week_start = 1) == week_day)) {
    stop("Input dates must be on the specified week day")
  }

  # assign each input date to a season
  seasons <- date_to_season(
    x,
    start_month = season_start_month,
    start_day = season_start_day
  )

  # each season has a reference date
  ref_dates <- ref_date_in_season(
    seasons,
    season_start_month = season_start_month,
    season_start_day = season_start_day,
    ref_month = ref_month,
    ref_day = ref_day
  )
  target_ref_date <- ref_date_in_season(
    target_season,
    season_start_month = season_start_month,
    season_start_day = season_start_day,
    ref_month = ref_month,
    ref_day = ref_day
  )

  # each season's reference date corresponds to a reference week date
  ref_week_dates <- week_ceiling(ref_dates)
  target_ref_week_date <- week_ceiling(target_ref_date)

  # optimal number of weeks to offset each season
  offset_days <- (target_ref_week_date - ref_week_dates) / lubridate::ddays(1)
  offset_weeks <- round(offset_days / 7)

  # move each input date by the offset weeks
  x + lubridate::weeks(offset_weeks)
}
