% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/align_weeks.R
\name{align_weeks}
\alias{align_weeks}
\title{Align weeks across seasons}
\usage{
align_weeks(
  x,
  target_season,
  ref_month = 1,
  ref_day = 1,
  season_start_month = 7,
  season_start_day = 1,
  week_day = 6
)
}
\arguments{
\item{x}{vector of input dates}

\item{target_season}{integer season to align to (first year of two-year
season)}

\item{ref_month}{integer month of the reference date (1=January)}

\item{ref_day}{integer day of the reference date}

\item{season_start_month}{integer month of the season start (1=January)}

\item{season_start_day}{integer day of the season start}

\item{week_day}{integer day of the week to align to (1=Monday)}
}
\value{
vector of dates, of same length as \code{x}
}
\description{
Align weeks across seasons
}
