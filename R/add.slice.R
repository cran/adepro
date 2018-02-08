#' add.slice - Function that adds single slices to a pie chart
#'
#' @description
#' Creates a polygon (base R) representing a slice in a the piechart
#'
#' @param no Number of the AE for the given patient, determining color of the AE (numeric)
#' @param r Radius of the circle
#' @param nop Total number of different AEs (numeric)
#' @param pos.x Center of the circle on x-lab (numeric)
#' @param pos.y Center of the circle on y-lab (numeric)
#' @param density Density of slice coloring (numeric)
#' @param colors Colors for slices (this should be maximally 8) (character)
#'
#' @keywords internal

add.slice <- function(no, r, pos.x, pos.y, nop, density = NULL,
                      colors=c("#e43157", "#377eb8", "#4daf4a", "#984ea3",
                               "#ff7f00", "#ffff33", "#a65628", "#f781bf")) {
  c <- colors[1:nop]
  x <- c(pos.x, pos.x + r * 0.9 * cos(seq(pi/2 - 2*pi / 8 * (no-1), pi/2 - 2*pi / 8 * no, length = 25)))
  y <- c(pos.y, pos.y + r * 0.9 * sin(seq(pi/2 - 2*pi / 8 * (no-1), pi/2 - 2*pi / 8 * no, length = 25)))
  polygon(x, y, col = c[no], border = c[no], density = density, pch = ".", lwd = 2)
}