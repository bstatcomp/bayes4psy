% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linear_class.R
\name{plot_fit,linear_class-method}
\alias{plot_fit,linear_class-method}
\alias{plot_fit_linear}
\title{plot_fit}
\usage{
\S4method{plot_fit}{linear_class}(object, ...)
}
\arguments{
\item{object}{linear_class object.}

\item{...}{subjects - plot fits on a subject level (default = TRUE).}
}
\value{
A ggplot visualization.
}
\description{
\code{plot_fit} plots fitted model against the data. Use this function to explore the quality of your fit. You can plot on the subject level (subjects=TRUE) or on the group level (subjects=FALSE).
}
\examples{
# to use the function you first have to prepare the data and fit the model
# see class documentation for an example of the whole process
# along with an example of how to use this function
?linear_class

}
