#' Example data set with simulated adverse event information
#'
#' @format A data frame with the following variables (in this order):
#' \describe{
#'   \item{day_start}{study day of start of the adverse event}
#'   \item{day_end}{study day of end of the adverse event}
#'   \item{patient}{subject ID}
#'   \item{ae}{adverse event (AE) term}
#'   \item{sev}{severity of the adverse event (1-mild, 2-moderate, 3-severe)}
#'   \item{trtem}{treatment-emergent adverse event (1-yes, 0-no)}
#'   \item{ser}{serious adverse event (1-yes, 0-no)}
#' }
"ae_data"
