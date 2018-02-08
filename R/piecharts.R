#' piecharts - function to depict all piecharts in one graphic given data files
#'
#' @description
#' Creates base R plot of all piecharts combined
#'
#' @param i Study day (numeric)
#' @param aes Adverse event to be included (character)
#' @param ae_data Adverse event dataset on day i (data.frame)
#' @param d_data Adverse event dataset before day i (data.frame)
#' @param patients Dataset with clinical annotation for patients (data.frame)
#' @param xlines Positions for x-axis separation lines (numeric)
#' @param ylines Positions for y-axis separation lines (numeric)
#'
#' @keywords internal

piecharts <- function(i, aes, ae_data, d_data, patients, xlines, ylines) {
  ae_data$ae <- as.numeric(factor(as.character(ae_data$ae), levels = aes))
  d_data$ae <- as.numeric(factor(as.character(d_data$ae), levels = aes))
  d_data$d <- rep(0, nrow(d_data))
  
  sj <- sapply(1:length(patients$ps), function(j) {
    rows <- rbind(d_data[which(d_data$patient == patients$ps[j]),],
                  ae_data[which(ae_data$patient == patients$ps[j]),])
    
    ## Set circle color (alive, dead, dropout)
    cont <- ifelse(i > patients$end[j], 3, 1)
    if (i >= patients$death[j]) cont <- 2
    
    piechart(rows$r, rows$ae, cont = cont, nop = length(aes),
             pos.x = patients$X[j], pos.y = patients$Y[j],
             density = rows$d)
  })
  
  # add lines:
  matplot(xlines[[1]], ylines[[1]], type = "l", lty = 2, col = "#6b6b6b", add = TRUE)
}