#' Round up a date to a week-ending date
#'
#' Note that `lubridate::ceiling_date()` will round up date-time
#' objects, even with `change_on_boundary = FALSE`. This function
#' checks that the input is a date, not a date-time.
#'
#' @param x vector of dates
#' @param week_day integer day of the week to round up to (1=Monday)
#'
#' @return vector of dates, of same length as `x`
week_ceiling <- function(x, week_day = 6) {
  if (!methods::is(x, "Date")) {
    stop("`x` must be a Date object")
  }
  lubridate::ceiling_date(
    x,
    unit = "weeks",
    week_start = week_day,
    change_on_boundary = FALSE
  )
}
