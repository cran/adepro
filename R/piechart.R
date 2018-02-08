#' piechart - creates piechart of AEs for on patient
#'
#' @description
#' Creates entire pie chart (base R) for one patient
#'
#' @param radii Radii of the single slices added to the pie chart (numeric)
#' @param nos Numbers of AEs to be included in pie chart (numeric)
#' @param nop Total number of different AEs (numeric)
#' @param pos.x Center of the circle on x-lab (numeric)
#' @param pos.y Center of the circle on y-lab (numeric)
#' @param c.col Colour of the circle (hexadecimal character)
#' @param cont Indicator of patient status
#' @param density Density of slice coloring (numeric)
#'
#' @keywords internal

piechart <- function(radii, nos, nop = 8, pos.x = 0, pos.y = 0,
                     c.col = "#383838", circle.radius=.9, cont = 1,
                     density = NULL) {
  patient.status <- c(alive="#383838", dead="black", dropout="#424242")
  par(pty="s", bg = "#424242")
  shape::plotcircle(c(pos.x, pos.y), r = circle.radius, col = patient.status[cont],
                    lcol = c.col, pch = ".", lwd = 4)
  
  if (length(nos) > 0) {
    pplot <- sapply(1:length(nos), function(i) add.slice(nos[i], radii[i], pos.x, pos.y,
                                                         nop = nop, density = density[i]))
  }
}