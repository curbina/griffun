% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MLB.R
\name{HRtracker_leaderboard}
\alias{HRtracker_leaderboard}
\title{Scrape Home Run Tracker leadersboards}
\usage{
HRtracker_leaderboard(bat_pitch = "bat")
}
\arguments{
\item{bat_pitch}{either 'bat' or 'pit'}
}
\description{
Scrape Home Run Tracker leadersboards
}
\details{
Limitations: Cannot change year. Always current
}
\examples{
hrt<-HRtracker_leaderboard('bat')
}
