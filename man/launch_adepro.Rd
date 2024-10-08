% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/launch_adepro.R
\name{launch_adepro}
\alias{launch_adepro}
\title{launch_adepro - Launches the AdEPro application}
\usage{
launch_adepro(host = "127.0.0.1", port = NULL, browser = NULL)
}
\arguments{
\item{host}{host link (defaults to the local machine "127.0.0.1")}

\item{port}{port number (randomly chosen unless specified as a certain number)}

\item{browser}{path to browser exe (defaults to standard browser)}
}
\value{
A shiny app
}
\description{
Starts the AdEPro application in the client's browser.
}
\details{
Further information on how to use this application can be found in the vignette of this package.
}
\examples{
\dontrun{
## Launch application on localhost (127.0.0.1)
## -------------------------------------------
## By default launch_adepro starts the application on localhost
## and a randomly selected port (e.g. 9876), in which case you can connect
## to the running application by navigating your browser to
## http://localhost:9876.
launch_adepro()

## Launch application on a different host
## --------------------------------------
## You can also run the application on a different host
## by specifying a hostname and port. Just make sure to
## use an open port on your machine. Here "open" means
## that the port should not be used by another service
## and the port is opened by your firewall.
launch_adepro(host="your-hostname", port=8888)


## Make the application available to your coworkers
## ------------------------------------------------
## within your local area network even without a
## dedicated Shiny server. The value set through the
## host argument says to accept any connection (not just from localhost).
## Then take note of your local IP (if you are under linux,
## you can see it through ifconfig). Say your IP is 192.168.1.70.
## Your colleagues can use your app by inserting in the address
## bar of their browser 192.168.1.70:8888, i.e. your IP followed
## by : and the port number you selected.
launch_adepro(host="0.0.0.0", port=8888)

## Launch application on a different browser
## ----------------------------------------
## To run the shiny app on a different browser than your standard browser
## use the "browser" argument to set the path to the respective .exe file.
launch_adepro(browser = "C:/Program Files/Mozilla Firefox/firefox.exe")


## launching the application.
}

}
\keyword{adepro}
