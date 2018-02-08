#' preproc_patients - Preprocessing Patient Data
#'
#' @description
#' Preprocesses patient dataset
#'
#' @param patients patient dataset
#' @param height number of circles to be displayed on the vertical axis
#'
#' @keywords internal

preproc_patients <- function(patients, height) {
  # set patient positions in layout of circles:
  vec_lay    <- set_vector_layout(patients, height)
  width      <- set_width(patients, height)
  patients$X <- rep(2*cumsum(width)-1, each = height)[which(vec_lay != 0)]
  patients$Y <- rep(seq(-1,-2*height+1, by = -2), length(width))[which(vec_lay != 0)]
  return(patients)
}
