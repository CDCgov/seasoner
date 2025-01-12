#' Round up a date to a week-ending date
#'
#' @param x vector of dates
#' @param week_day integer day of the week to round up to (1=Monday)
week_ceiling <- function(x, week_day = 6) {
  if (!is(x, "Date")) {
    stop("`x` must be a Date object")
  }
  ifelse(
    lubridate::wday(x, week_start = 1) == week_day,
    x,
    lubridate::ceiling_date(x, unit = "weeks", week_start = week_day)
  )
}
