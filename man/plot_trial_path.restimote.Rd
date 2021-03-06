% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/restimote-visualising.R
\name{plot_trial_path.restimote}
\alias{plot_trial_path.restimote}
\title{plots a single trial path}
\usage{
\method{plot_trial_path}{restimote}(obj, trialId, until_reached = T)
}
\arguments{
\item{obj}{RestimoteObject Needs to be preprocessed}

\item{trialId}{vector integer of given trial}
}
\value{
prints plot
}
\description{
plots a single trial path
}
\examples{
plot_trial_path(obj, 1)
}
