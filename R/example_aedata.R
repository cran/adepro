#' Example data set with simulated adverse event information
#'
#' @format A data frame with the following variables (in this order):
#' \describe{
#'   \item{SUBJIDN}{subject ID}
#'   \item{AEDECOD}{adverse event (AE) term}
#'   \item{AESTDY}{study day of start of the adverse event}
#'   \item{AEENDY}{study day of end of the adverse event}
#'   \item{AESEVN}{severity of the adverse event (1-mild, 2-moderate, 3-severe)}
#'   \item{AESERN}{serious adverse event}
#'   \item{AETRTEMN}{treatment-emergent adverse event (1-yes, 0-no)}
#'   \item{AERELN}{serious adverse event (1-yes, 0-no)}
#'   \item{TRT01A}{treatment group}
#'   \item{TRTSDT}{study day of treatment start}
#'   \item{LVDT}{study day of drop out}
#'   \item{DTHDT}{study day of death}
#'   \item{AGE}{subject age}
#'   \item{SEX}{subject sex}
#'   \item{REGION}{subject region}
#'   \item{SAFFN}{safety flag (numeric)}
#' }
"example_aedata"
