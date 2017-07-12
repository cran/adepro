#' launch_adepro - Launches the AdEPro application
#'
#' @export
#'
#' @description
#' Starts the AdEPro application in the client's browser.
#'
#' @param host host link (defaults to the local machine "127.0.0.1")
#' @param port port number (randomly chosen unless specified as a certain number)
#' @param browser path to browser exe (defaults to standard browser)
#'
#' @keywords adepro
#'
#' @details Further information on how to use this application can be found in the vignette of this package.
#'
#' @examples
#' \dontrun{
#' ## Launch application on localhost (127.0.0.1)
#' ## -------------------------------------------
#' ## By default launch_adepro starts the application on localhost
#' ## and a randomly selected port (e.g. 9876), in which case you can connect
#' ## to the running application by navigating your browser to
#' ## http://localhost:9876.
#' launch_adepro()
#'
#' ## Launch application on a different host
#' ## --------------------------------------
#' ## You can also run the application on a different host
#' ## by specifying a hostname and port. Just make sure to
#' ## use an open port on your machine. Here "open" means
#' ## that the port should not be used by another service
#' ## and the port is opened by your firewall.
#' launch_adepro(host="your-hostname", port=8888)
#'
#'
#' ## Make the application available to your coworkers
#' ## ------------------------------------------------
#' ## within your local area network even without a
#' ## dedicated Shiny server. The value set through the
#' ## host argument says to accept any connection (not just from localhost).
#' ## Then take note of your local IP (if you are under linux,
#' ## you can see it through ifconfig). Say your IP is 192.168.1.70.
#' ## Your colleagues can use your app by inserting in the address
#' ## bar of their browser 192.168.1.70:8888, i.e. your IP followed
#' ## by : and the port number you selected.
#' launch_adepro(host="0.0.0.0", port=8888)
#'
#' ## Launch application on a different browser
#' ## ----------------------------------------
#' ## To run the shiny app on a different browser than your standard browser
#' ## use the "browser" argument to set the path to the respective .exe file.
#' launch_adepro(browser = "C:/Program Files/Mozilla Firefox/firefox.exe")
#' }
#'
#' @import graphics
#' @import utils
#' @import shiny
#' @import shinyBS
#' @import MASS
#' @import audio
#' @import shape
#' @import Cairo
#' @return A shiny app
launch_adepro <- function(host = "127.0.0.1", port = NULL, browser=NULL) {

  ######################################################################################################
  ################################### Initializing Helper Functions ####################################
  ######################################################################################################

  # InitQ - initiates classification matrix of AEs (treatment-emergent, serious etc.)
  initQ <- function(ae_data) {
    if (ncol(ae_data) == 5) {Q <- data.frame(trtem = rep(TRUE, nrow(ae_data)))}
    if (ncol(ae_data) == 6) {Q <- as.data.frame(as.logical(ae_data[,-c(1:5)])); colnames(Q) <- colnames(ae_data)[-c(1:5)]}
    if (ncol(ae_data) > 6)  {Q <- apply(ae_data[,-c(1:5)], 2, as.logical)}
    return(Q)
  }

  # initAE_options - initiates descriptions of AE classifications
  initAE_options <- function(ae_data, Q) {
    AE_options <- 1:ncol(Q)
    type_names <- data.frame(short=c("trtem", "ser", "nonser", "studrel", "studrelser", "relprot", "resdisc", "studrelresdisc"),
                             long=c("all treatment-emergent", "serious ", "non-serious ", "study drug-related ",
                                    "study-drug related and serious", "related to procedures required by the protocol",
                                    "resulting in discontinuation of study drug",
                                    "study drug-related and resulting in discontinuation of study drug"))
    names(AE_options) <- sapply(1:ncol(Q), function(x) type_names$long[which(type_names$short == colnames(Q)[x])])
    return(AE_options)
  }

  # check_data - Consistency checks on the data
  check_data <- function(ae_data, patients) {
    # Basic parameter checks:
    if (!is.data.frame(ae_data)) stop("ae_data has to be a data frame")
    if (!is.data.frame(patients)) stop("patients has to be a data frame")
    if(!all(is.element(unique(ae_data$patient), unique(patients$ps)))) {stop("Patient IDs do not match!")}
    if (any(colnames(ae_data)[1:5] != c("day_start", "day_end", "patient", "ae", "sev"))) stop("columns in ae_data are not named correctly")
    if (any(!(colnames(ae_data)[-c(1:5)] %in%
              c("trtem", "ser", "nonser", "studrel", "studrelser", "relprot", "resdisc", "studrelresdisc")))) stop("columns in ae_data are not named correctly")
    if (any(colnames(patients) != c("ps", "treat", "end", "death"))) stop("columns in patients are not named correctly")
    if (!is.factor(ae_data[,4])) stop("ae_data$ae has to be of type of factor")
    if (!all(c(apply(ae_data[,-4], 2, is.numeric), apply(patients[,-2], 2, is.numeric)))) stop("all variables expect 'ae' and 'treat' have to be of type numeric")
    if (!all(ae_data$sev %in% c(1,2,3))) stop("ae_data$sev must include only numbers from 1 to 3")
    if (any(ae_data$day_start < 1 | ae_data$day_end < 1)) stop("'day_start' and 'day_end' must be 1 or greater")
    if (any(ae_data$day_start > ae_data$day_end)) stop("'day_end' must be greater or equal to 'day_start'")
  }

  # preproc_ae - Preprocessing adverse event data
  preproc_ae <- function(ae_data) {
    ae_data <- ae_data[,1:5]
    denom <- 4
    ae_data$r <- (ae_data$sev + 1) / denom
    ae_data$d <- rep(NA, nrow(ae_data))
    return(ae_data)
  }

  # preproc_patients - Preprocessing Patient Data
  preproc_patients <- function(patients, height) {
    patients <- set_patient_pos(patients, height)
    return(patients)
  }

  # set_trt - calculates number of subjects per treatment group
  set_trt <- function(patients) {
    treatment <- sapply(unique(patients$treat), function(x) length(which(patients$treat == x)))[-1]
    return(treatment)
  }

  # set_trt1 - calculates number of subjects in the first treatment group
  set_trt1 <- function(patients) {
    treat_n <- as.numeric(factor(patients$treat, levels = unique(patients$treat)))
    treatment <- length(which(treat_n == min(treat_n)))
    return(treatment)
  }

  # set_vector_layout - creates vector layout
  set_vector_layout <- function(patients, height) {
    l_trt  <- set_trt(patients); l_trt1 <- set_trt1(patients)
    vec_lay <- c(1:l_trt1, rep(0, ifelse(l_trt1%%height==0, 0, height-l_trt1%%height)))
    if (length(l_trt) > 0) {
      for (z in 1:length(l_trt)) {
        if (l_trt[z]%%height==0) diff <- 0 else diff <- height-l_trt[z]%%height
        vec_lay_add <- c((1:l_trt[z])+max(vec_lay), rep(0, diff))
        vec_lay <- c(vec_lay, vec_lay_add)
      }
    }
    return(vec_lay)
  }

  # set_width - calculates width (number of columns with circles)
  set_width <- function(patients, height) {
    vec_lay <- set_vector_layout(patients, height)
    width <- rep(1, ceiling(length(vec_lay)/height))
    return(width)
  }

  # set_patient_pos - sets patient positions in layout of circles
  set_patient_pos <-  function(patients, height) {
    vec_lay    <- set_vector_layout(patients, height)
    width      <- set_width(patients, height)
    patients$X <- rep(2*cumsum(width)-1, each = height)[which(vec_lay != 0)]
    patients$Y <- rep(seq(-1,-2*height+1, by = -2), length(width))[which(vec_lay != 0)]
    return(patients)
  }

  # set_group_lines - determines coordinates for the separating lines between treatment groups
  set_group_lines <- function(patients, height) {
    xlines <- 0; ylines <- 0
    l_trt  <- set_trt(patients); l_trt1 <- set_trt1(patients)
    plines <- ceiling(c(l_trt1 / height, l_trt / height)) * 2
    if (length(l_trt) > 0) {
      xlines <- matrix(rep(cumsum(plines)[-length(plines)], each = 2), nrow = 2)
      ylines <- matrix(rep(c(-2 * height, 0), each = ncol(xlines)), nrow = 2, byrow = TRUE)
    }
    return(list(xlines, ylines, plines))
  }

  # setFootnote - Setter for footnote
  setFootnote <- function(patients) {
    footnote <- paste("SAF (N=", nrow(patients), ")", sep="")
    return(footnote)
  }

  # set_global_params - sets all global parameters
  set_global_params <- function(ae_data, patients, title=NULL, height=NULL) {

    ## Consistency checks
    if(is.null(title)) {
      title <- rep("", length(unique(patients$treat)))
    }
    if (is.null(height)) {
      height <- ceiling(sqrt(850 / 1920 * nrow(patients)))
    }
    check_data(ae_data, patients)

    ## Local variables
    Q <- initQ(ae_data)
    xylines <- set_group_lines(patients, height)
    xlines  <- xylines[1]
    ylines  <- xylines[2]
    plines  <- xylines[3]
    globals <- list(titles=title,
                    footnote=setFootnote(patients),
                    Q=Q,
                    AE_options=initAE_options(ae_data, Q),
                    width=set_width(patients, height),
                    height=height,
                    xlines=xlines,
                    ylines=ylines,
                    plines=plines)
    return(globals)
  }

  ######################################################################################################
  ############################ Audio-Visualizing Helper Functions ######################################
  ######################################################################################################

  # add.slice - Function that adds single slices to a pie chart
  # Input:
  # no - Number of the AE for the given patient, determining color of the AE (numeric)
  # r - Radius of the circle
  # nop - Total number of different AEs (numeric)
  # pos.x / pos.y - Center of the circle on x/y-lab (numeric)
  # density - Density of slice coloring (numeric)
  # colors - Colors for slices (this should be maximally 8) (character)
  # Output: A polygon (base R) representing a slice in a the piechart
  add.slice <- function(no, r, pos.x, pos.y, nop, density = NULL,
                        colors=c("#e43157", "#377eb8", "#4daf4a", "#984ea3",
                                 "#ff7f00", "#ffff33", "#a65628", "#f781bf")) {
    c <- colors[1:nop]
    x <- c(pos.x, pos.x + r * 0.9 * cos(seq(pi/2 - 2*pi / 8 * (no-1), pi/2 - 2*pi / 8 * no, length = 25)))
    y <- c(pos.y, pos.y + r * 0.9 * sin(seq(pi/2 - 2*pi / 8 * (no-1), pi/2 - 2*pi / 8 * no, length = 25)))
    polygon(x, y, col = c[no], border = c[no], density = density, pch = ".", lwd = 2)
  }

  # piechart - creates piechart of AEs for on patient
  # Input:
  # radii - Radii of the single slices added to the pie chart (numeric)
  # nos - Numbers of AEs to be included in pie chart (numeric)
  # nop - Total number of different AEs (numeric)
  # pos.x / pos.y - Center of the circle on x/y-lab (numeric)
  # pos.y Center of the circle on y-lab (numeric)
  # c.col - Colour of the circle (hexadecimal character)
  # cont - Indicator of patient status
  # density - Density of slice coloring (numeric)
  # Output: Entire pie chart (base R) for one patient
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

  # piecharts - Function to depict all piecharts in one graphic given data files
  # Input:
  # i - Study day (numeric)
  # aes - Adverse event to be included (character)
  # ae_data - Adverse event dataset on day i (data.frame)
  # d_data - Adverse event dataset before day i (data.frame)
  # patients - Dataset with clinical annotation for patients (data.frame)
  # xlines - Positions for x-axis separation lines (numeric)
  # ylines - Positions for y-axis separation lines (numeric)
  # Output: Base R plot of all piecharts combined
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


  # pie_legend - Function to create legend for displayed adverse events
  # aes - chosen adverse events to display (character)
  # Output: A legend object for base R plots
  pie_legend <- function(aes, colors=c("#e43157", "#377eb8", "#4daf4a", "#984ea3",
                                       "#ff7f00", "#ffff33", "#a65628", "#f781bf")) {
    par(oma=c(0,0,0,0), mar = c(0,0,0,0), font = 1)
    u <- par("usr")
    rect(u[1], u[3], u[2], u[4], col = "#383838", border = "#383838")
    if (length(aes) > 0) {
      legend("topleft", legend = aes, col = colors[1:length(aes)],
             lwd = 15, cex = 1, bty = "n",
             bg = "#383838", box.col = "#383838", text.col = "#ffffff")
    }
  }


  # tone - Function that plays two different sounds
  # x - Vector of two logical values: if first / second element is true, first /second sound is played ()
  # d - Duration of the sounds (numeric)
  # tG - Frequency of first sound
  # tC - Frequency of second sound
  # Output: A sound
  tone <- function(x, d = 0.5, tG = 391.995, tC = 261.626) {
    audio::play(audio::audioSample(as.numeric(matrix(c(x[1]*sin(2*pi*tG*seq(0,d,length.out=d*16000)),
                                                x[2]*sin(2*pi*tC*seq(0,d,length.out=d*16000))),
                                              ncol = 2)[,which(x!=0)]), 3 * 16000))

  }

  ######################################################################################################
  ########################################## Server ####################################################
  ######################################################################################################

  options(shiny.usecairo = T)

  server <- shinyServer(function(input, output, session) {

    # Reactive variables:
    ae_data <- reactive({
      inFile <- input$ae_dat
      if (is.null(inFile)) return(NULL)
      data <- read.csv(inFile$datapath, header = TRUE, sep = ",", stringsAsFactors = T)
      return(data)
    })

    patient_data <- reactive({
      inFile <- input$patient_dat
      if (is.null(inFile)) return(NULL)
      data <- read.csv(inFile$datapath, header = TRUE, sep = ",", stringsAsFactors = T)
      return(data)
    })

    has_data <- reactive({
      return(!(is.null(ae_data) & is.null(patient_data)))
    })

    global_params <- reactive({
      title <- as.character(unique(patient_data()$treat))
      globals <- set_global_params(ae_data(), patient_data(),
                                   title=title)
      return(globals)
    })

    patients <- reactive({
      global_params <- global_params()
      patient_data  <- patient_data()
      patient_data  <- preproc_patients(patient_data, global_params$height)
      return(patient_data)
    })

    data_raw <- reactive({
      ae_data <- ae_data()
      Q <- initQ(ae_data)
      ae_data <- preproc_ae(ae_data)
      ae_data <- ae_data[which(Q[, as.numeric(input$type)]), ]
      return(ae_data)
    })

    data <- reactive({
      data <- data_raw()
      selected <- data_raw()$ae %in% input$var
      data <- data[selected, ]
      return(data)
    })

    ## Select all data before selected day
    ddata <- reactive({
      data <- data()
      selected <- data$day_end < input$day
      data <- data[selected, ]
      return(data)
    })

    data.sel <- reactive({
      data <- data()
      selected <- (data$day_start <= input$day) & (input$day <= data$day_end)
      data <- data[selected, ]
      return(data)
    })

    all_aes <- reactive({
      dat <- data_raw()
      ae_table <- sort(table(rep(dat$ae, dat$day_end - dat$day_start + 1)), decreasing = TRUE)
      ae_table <- ae_table[ae_table > 0]
      aes <- names(ae_table)
      return(aes)
    })

    # UI elements
    output$ae_select <- renderUI({
      if (is.null(ae_data()) | is.null(patient_data())) {
        return(NULL)
      }

      most_freq_aes <- head(all_aes(), 8)
      uiElement <- selectizeInput("var", label = "Choose Adverse Events for display (max. 8):", options  = list(maxItems = 8),
                                  choices  = all_aes(), selected = most_freq_aes, multiple = TRUE)
      uiElement2 <- selectInput("audio",
                                label = "Choose Adverse Event for audio:",
                                choices = c("- no audio -", all_aes()),
                                selected = "- no audio -")
      return(list(uiElement, uiElement2))
    })

    output$day_slider <- renderUI({
      if (is.null(ae_data()) | is.null(patient_data())) {
        return(NULL)
      }

      ae_data <- data_raw()
      day_max <- ifelse(length(ae_data$day_end)==0,
                        1, max(ae_data$day_end))
      uiElement <- sliderInput("day",
                               label = NULL,
                               min = 1,
                               max = day_max,
                               value = 1, step = 1,
                               animate = animationOptions(interval = 2000),
                               width = '100%')
      return(uiElement)
    })

    output$ae_type <- renderUI({
      if (is.null(ae_data()) | is.null(patient_data())) {
        return(NULL)
      }

      global_params <- global_params()
      uiElement <- selectizeInput("type",
                                  label    = NULL,
                                  choices  = global_params$AE_options,
                                  selected = 1)
      return(uiElement)
    })

    output$file_upload <- renderUI({
      uiElement <- list(fileInput('ae_dat', 'Upload Adverse Event Data',
                                  accept = c(
                                    'text/csv',
                                    'text/comma-separated-values',
                                    'text/tab-separated-values',
                                    'text/plain',
                                    '.csv',
                                    '.tsv')),
                        fileInput('patient_dat', 'Upload Patient Data',
                                  accept = c(
                                    'text/csv',
                                    'text/comma-separated-values',
                                    'text/tab-separated-values',
                                    'text/plain',
                                    '.csv',
                                    '.tsv')))
      return(uiElement)
    })

    shiny::observeEvent(has_data(), ({
                          shinyBS::updateCollapse(session,
                                                  id = "collapse",
                                                  open = c(
                                                    "Type of Adverse Event",
                                                    "Adverse Events for animation"))}))

    shiny::observeEvent(input$type, ({
                          shinyBS::updateCollapse(session,
                                                  id = "collapse",
                                                  open = "Adverse Events for animation")}))

    # plots:
    output$slice_plots <- renderPlot({
      if (is.null(ae_data()) | is.null(patient_data())) {
        return(NULL)
      }

      patients <- patients()
      global_params <- global_params()
      height <- global_params$height
      width  <- global_params$width
      xlines <- global_params$xlines
      ylines <- global_params$ylines
      plines <- global_params$plines[[1]]
      xval <- c(0, cumsum(plines)[-length(plines)]) + plines/2
      title <- as.character(unique(patient_data()$treat))

      par(oma=c(0, 0, 0, 0), mar=c(0, 0, 0, 0))
      MASS::eqscplot(patients$X, patients$Y, tol = 0, axes = F,
                     xlab = "", ylab = "", main = "",
                     xlim = c(0, 2*sum(width)),
                     ylim = c(-2*height, 3), type = "n")
      text(x = xval, y = rep(1.5, length(xval)), labels = title, col = "#6b6b6b", cex = 2)
      if (length(input$var) > 0) {
        piecharts(i = input$day, aes = input$var,
                  ae_data = data.sel(),
                  d_data = ddata(),
                  patients,
                  xlines, ylines)
        if (input$audio != "- no audio -") {
          tf1 <- (0 != sum(ae_data()$ae  == input$audio & ae_data()[, 1] == input$day))
          tf2 <- (0 != sum(ae_data()$ae  == input$audio & ae_data()[, 2]+1 == input$day))
          tone(c(tf1, tf2))
        }
      }
    }, bg="#424242")

    output$plot_hoverinfo <- renderPrint({
      if (is.null(ae_data()) | is.null(patient_data())) {
        return(cat("Please upload your data using the 'Upload Data' tab!"))
      }
      patients <- patients()
      global_params <- global_params()
      footnote <- global_params$footnote

      cat(footnote, "/ Subject ID:",
          nearPoints(patients, input$plot_hover,
                     threshold = 30, maxpoints = 1,
                     xvar = "X", yvar = "Y")$ps)
    })

    output$legend <- renderPlot({
      if (is.null(ae_data()) | is.null(patient_data())) {
        return(NULL)
      } else {
        return(pie_legend(aes = input$var))
      }
    }, bg = "#383838")

    output$dayinfo <- renderPrint({
      if (is.null(ae_data()) & is.null(patient_data())) {
        return(cat(""))
      } else {
        cat(input$day)
      }
    })
  })


  ######################################################################################################
  ####################################### User Interface (UI) ##########################################
  ######################################################################################################

  ui <- shinyUI(fluidPage(
    ## CSS codes:
    tags$style(type = "text/css", "
               .irs-bar {width: 100%; height: 25px; background: #55126184; border-top: 1px solid #55126184; border-bottom: 1px solid #55126184;}
               .irs-bar-edge {background: #55126184; border: 1px solid #55126184; height: 25px; border-radius: 5px; width: 20px;}
               .irs-line {border: 1px solid #55126184; height: 25px; border-radius: 5px;}
               .irs-grid-text {font-family: 'arial'; color: black; bottom: 17px; z-index: 1;font-size:15px}
               .irs-grid-pol {display: none;}
               #.irs-max {font-family: 'arial'; color: #ffffff; height:15px; font-size:15px}
               #.irs-min {font-family: 'arial'; color: #ffffff; height:15px; font-size:15px}
               .irs-max {display:none}
               .irs-min {display:none}
               .irs-single {color:#ffffff; background:#e43157; height:20px; font-size:15px;}
               .irs-slider {width: 30px; height: 30px; top: 22px;}
               .panel-group.sbs-panel-group{position: absolute;width: 90%;}
               .panel.panel-default{background-color:#383838; color:#ffffff;border-color:#6b6b6b}
               .panel-heading{color:#ffffff;padding:0;}
               .panel-title{background:#383838; color:#ffffff;margin-top:10px;margin-bottom:10px;padding-left:5px}
               body {background-color: #424242; color: #6b6b6b}
               .panel-group.sbs-panel-group {position: absolute; width: 90%;}
               .panel.panel-default {background-color: #383838; color: #ffffff; border-color: #6b6b6b}
               .panel-heading {color: #ffffff; padding: 0;}
               .panel-title {background: #383838;color: #ffffff; margin-top: 10px; margin-bottom: 10px; padding-left: 5px}
               .myRow1 {background-color: #383838; height: 170px;}
               .myRow3 {background-color: #424242;}
               #dayinfo {background-color: #383838; color: #ffffff; border-color: #383838; font: Arial; font-size: 50px;}
               #plot_hoverinfo {background-color: #383838; color: #6b6b6b; border-color: #383838; font-size: 16px;}
               "),

    # Main panel
    mainPanel(
      fluidRow(class = "myRow1",
               column(2, br(),
                      shinyBS::bsCollapse(
                        shinyBS::bsCollapsePanel("Upload Data", uiOutput("file_upload")),
                        shinyBS::bsCollapsePanel("Type of Adverse Event", uiOutput("ae_type")),
                        shinyBS::bsCollapsePanel("Adverse Events for animation", uiOutput("ae_select")),
                        multiple = TRUE, id = "collapse")),
               column(5, br(),
                      uiOutput("day_slider"),
                      verbatimTextOutput("plot_hoverinfo")),
               column(1, br(),
                      verbatimTextOutput("dayinfo")),
               column(2,
                      plotOutput("legend", width = "200px", height = "130px")),
               column(2,
                      #img(src='adepro_small.png', align = "right", width = "115px", height = "80px")
                      tags$div(HTML('<?xml version="1.0" encoding="utf-8"?>
                                    <!-- Generator: Adobe Illustrator 21.0.2, SVG Export Plug-In . SVG Version: 6.00 Build 0)  -->
                                    <svg version="1.1" id="Ebene_1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px" y="0px"
                                    viewBox="0 0 1080 1080" style="enable-background:new 0 0 1080 1080;width:230px;height:160px;" align="right" xml:space="preserve">
                                    <style type="text/css">
                                    .st0{fill-rule:evenodd;clip-rule:evenodd;fill:#377EB8;}
                                    .st1{fill-rule:evenodd;clip-rule:evenodd;fill:#E43157;}
                                    .st2{fill:#FFFFFF;}
                                    </style>
                                    <path class="st0" d="M547.7,321.2h28V664H219v-14C219,468.4,366.1,321.2,547.7,321.2z"/>
                                    <path class="st1" d="M583.4,758.8h-7.7v-94.8h98.7v3.9C674.4,718,633.6,758.8,583.4,758.8z"/>
                                    <path class="st2" d="M444.8,632.2c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S446.1,632.2,444.8,632.2z M433.1,503.3c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S434.4,503.3,433.1,503.3z M444.8,597c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S446.1,597,444.8,597z M444.8,608.7c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S446.1,608.7,444.8,608.7z M444.8,620.5c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S446.1,620.5,444.8,620.5z M433.1,550.1c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S434.4,550.1,433.1,550.1z M433.1,561.9c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S434.4,561.9,433.1,561.9z M433.1,538.4c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S434.4,538.4,433.1,538.4z M433.1,573.6c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S434.4,573.6,433.1,573.6z M433.1,526.7c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S434.4,526.7,433.1,526.7z M444.8,515c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S446.1,515,444.8,515z M444.8,503.3c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S446.1,503.3,444.8,503.3z M444.8,526.7c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S446.1,526.7,444.8,526.7z M444.8,585.3c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S446.1,585.3,444.8,585.3z M456.5,632.2c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S457.8,632.2,456.5,632.2z M456.5,620.5c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S457.8,620.5,456.5,620.5z M444.8,573.6c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S446.1,573.6,444.8,573.6z M444.8,561.9c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S446.1,561.9,444.8,561.9z M444.8,550.1c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S446.1,550.1,444.8,550.1z M444.8,538.4c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S446.1,538.4,444.8,538.4z M397.9,550.1c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S399.2,550.1,397.9,550.1z M397.9,585.3c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S399.2,585.3,397.9,585.3z M409.6,608.7c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S410.9,608.7,409.6,608.7z M409.6,620.5c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S410.9,620.5,409.6,620.5z M409.6,597c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S410.9,597,409.6,597z M409.6,585.3c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S410.9,585.3,409.6,585.3z M397.9,538.4c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S399.2,538.4,397.9,538.4z M397.9,573.6c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S399.2,573.6,397.9,573.6z M397.9,561.9c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S399.2,561.9,397.9,561.9z M456.5,608.7c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S457.8,608.7,456.5,608.7z M409.6,573.6c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S410.9,573.6,409.6,573.6z M409.6,538.4c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S410.9,538.4,409.6,538.4z M421.4,538.4c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S422.7,538.4,421.4,538.4z M421.4,550.1c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S422.7,550.1,421.4,550.1z M433.1,620.5c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S434.4,620.5,433.1,620.5z M433.1,597c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S434.4,597,433.1,597z M433.1,608.7c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S434.4,608.7,433.1,608.7z M409.6,550.1c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S410.9,550.1,409.6,550.1z M409.6,561.9c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S410.9,561.9,409.6,561.9z M433.1,585.3c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S434.4,585.3,433.1,585.3z M421.4,632.2c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S422.7,632.2,421.4,632.2z M421.4,620.5c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S422.7,620.5,421.4,620.5z M409.6,632.2c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S410.9,632.2,409.6,632.2z M489.7,632.2c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S491,632.2,489.7,632.2z M489.7,573.6c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S491,573.6,489.7,573.6z M489.7,550.1c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S491,550.1,489.7,550.1z M489.7,526.7c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S491,526.7,489.7,526.7z M489.7,515c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S491,515,489.7,515z M489.7,538.4c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S491,538.4,489.7,538.4z M489.7,608.7c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S491,608.7,489.7,608.7z M489.7,620.5c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S491,620.5,489.7,620.5z M489.7,585.3c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S491,585.3,489.7,585.3z M489.7,597c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S491,597,489.7,597z M489.7,561.9c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S491,561.9,489.7,561.9z M501.4,573.6c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S502.7,573.6,501.4,573.6z M501.4,561.9c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S502.7,561.9,501.4,561.9z M501.4,550.1c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S502.7,550.1,501.4,550.1z M456.5,597c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S457.8,597,456.5,597z M501.4,538.4c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S502.7,538.4,501.4,538.4z M501.4,585.3c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S502.7,585.3,501.4,585.3z M501.4,620.5c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S502.7,620.5,501.4,620.5z M501.4,632.2c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S502.7,632.2,501.4,632.2z M501.4,608.7c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S502.7,608.7,501.4,608.7z M501.4,597c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S502.7,597,501.4,597z M489.7,503.3c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S491,503.3,489.7,503.3z M456.5,538.4c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S457.8,538.4,456.5,538.4z M456.5,526.7c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S457.8,526.7,456.5,526.7z M456.5,515c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S457.8,515,456.5,515z M478,632.2c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S479.3,632.2,478,632.2z M456.5,503.3c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S457.8,503.3,456.5,503.3z M456.5,573.6c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S457.8,573.6,456.5,573.6z M456.5,585.3c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S457.8,585.3,456.5,585.3z M478,503.3c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S479.3,503.3,478,503.3z M478,620.5c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S479.3,620.5,478,620.5z M456.5,561.9c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S457.8,561.9,456.5,561.9z M456.5,550.1c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S457.8,550.1,456.5,550.1z M478,538.4c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S479.3,538.4,478,538.4z M478,526.7c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S479.3,526.7,478,526.7z M478,550.1c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S479.3,550.1,478,550.1z M478,515c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S479.3,515,478,515z M478,561.9c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S479.3,561.9,478,561.9z M478,585.3c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S479.3,585.3,478,585.3z M478,597c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S479.3,597,478,597z M478,608.7c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S479.3,608.7,478,608.7z M478,573.6c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S479.3,573.6,478,573.6z M341.3,608.7c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S342.6,608.7,341.3,608.7z M306.1,526.7c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S307.4,526.7,306.1,526.7z M317.8,608.7c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S319.1,608.7,317.8,608.7z M317.8,585.3c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S319.1,585.3,317.8,585.3z M317.8,561.9c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S319.1,561.9,317.8,561.9z M317.8,573.6c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S319.1,573.6,317.8,573.6z M306.1,503.3c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S307.4,503.3,306.1,503.3z M317.8,550.1c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S319.1,550.1,317.8,550.1z M306.1,515c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S307.4,515,306.1,515z M317.8,620.5c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S319.1,620.5,317.8,620.5z M317.8,632.2c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S319.1,632.2,317.8,632.2z M317.8,597c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S319.1,597,317.8,597z M329.6,503.3c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S330.9,503.3,329.6,503.3z M329.6,515c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S330.9,515,329.6,515z M341.3,620.5c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S342.6,620.5,341.3,620.5z M329.6,561.9c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S330.9,561.9,329.6,561.9z M341.3,632.2c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S342.6,632.2,341.3,632.2z M317.8,503.3c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S319.1,503.3,317.8,503.3z M317.8,526.7c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S319.1,526.7,317.8,526.7z M317.8,515c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S319.1,515,317.8,515z M317.8,538.4c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S319.1,538.4,317.8,538.4z M329.6,573.6c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S330.9,573.6,329.6,573.6z M306.1,620.5c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S307.4,620.5,306.1,620.5z M294.4,573.6c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S295.7,573.6,294.4,573.6z M294.4,561.9c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S295.7,561.9,294.4,561.9z M294.4,538.4c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S295.7,538.4,294.4,538.4z M294.4,550.1c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S295.7,550.1,294.4,550.1z M294.4,585.3c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S295.7,585.3,294.4,585.3z M294.4,632.2c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S295.7,632.2,294.4,632.2z M294.4,620.5c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S295.7,620.5,294.4,620.5z M294.4,597c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S295.7,597,294.4,597z M294.4,526.7c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S295.7,526.7,294.4,526.7z M294.4,608.7c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S295.7,608.7,294.4,608.7z M306.1,585.3c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S307.4,585.3,306.1,585.3z M306.1,538.4c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S307.4,538.4,306.1,538.4z M306.1,573.6c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S307.4,573.6,306.1,573.6z M306.1,561.9c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S307.4,561.9,306.1,561.9z M306.1,550.1c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S307.4,550.1,306.1,550.1z M294.4,515c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S295.7,515,294.4,515z M501.4,526.7c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S502.7,526.7,501.4,526.7z M306.1,632.2c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S307.4,632.2,306.1,632.2z M306.1,597c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S307.4,597,306.1,597z M306.1,608.7c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S307.4,608.7,306.1,608.7z M364.7,515c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S366,515,364.7,515z M364.7,573.6c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S366,573.6,364.7,573.6z M364.7,561.9c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S366,561.9,364.7,561.9z M364.7,550.1c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S366,550.1,364.7,550.1z M364.7,526.7c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S366,526.7,364.7,526.7z M364.7,538.4c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S366,538.4,364.7,538.4z M397.9,597c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S399.2,597,397.9,597z M364.7,585.3c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S366,585.3,364.7,585.3z M364.7,608.7c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S366,608.7,364.7,608.7z M364.7,620.5c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S366,620.5,364.7,620.5z M364.7,632.2c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S366,632.2,364.7,632.2z M364.7,597c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S366,597,364.7,597z M397.9,632.2c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S399.2,632.2,397.9,632.2z M386.2,550.1c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S387.5,550.1,386.2,550.1z M386.2,620.5c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S387.5,620.5,386.2,620.5z M397.9,608.7c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S399.2,608.7,397.9,608.7z M397.9,620.5c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S399.2,620.5,397.9,620.5z M386.2,561.9c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S387.5,561.9,386.2,561.9z M386.2,573.6c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S387.5,573.6,386.2,573.6z M386.2,597c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S387.5,597,386.2,597z M341.3,597c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S342.6,597,341.3,597z M386.2,585.3c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S387.5,585.3,386.2,585.3z M386.2,608.7c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S387.5,608.7,386.2,608.7z M353,503.3c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S354.3,503.3,353,503.3z M341.3,515c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S342.6,515,341.3,515z M341.3,538.4c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S342.6,538.4,341.3,538.4z M353,632.2c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S354.3,632.2,353,632.2z M341.3,503.3c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S342.6,503.3,341.3,503.3z M341.3,526.7c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S342.6,526.7,341.3,526.7z M341.3,573.6c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S342.6,573.6,341.3,573.6z M341.3,585.3c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S342.6,585.3,341.3,585.3z M341.3,550.1c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S342.6,550.1,341.3,550.1z M341.3,561.9c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S342.6,561.9,341.3,561.9z M353,620.5c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S354.3,620.5,353,620.5z M353,526.7c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S354.3,526.7,353,526.7z M353,550.1c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S354.3,550.1,353,550.1z M353,538.4c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S354.3,538.4,353,538.4z M353,515c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S354.3,515,353,515z M353,597c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S354.3,597,353,597z M353,561.9c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S354.3,561.9,353,561.9z M353,585.3c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S354.3,585.3,353,585.3z M353,608.7c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S354.3,608.7,353,608.7z M353,573.6c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S354.3,573.6,353,573.6z M433.1,515c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S434.4,515,433.1,515z M536.6,561.9c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S537.9,561.9,536.6,561.9z M536.6,573.6c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S537.9,573.6,536.6,573.6z M536.6,515c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S537.9,515,536.6,515z M536.6,620.5c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S537.9,620.5,536.6,620.5z M524.9,503.3c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S526.2,503.3,524.9,503.3z M536.6,503.3c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S537.9,503.3,536.6,503.3z M536.6,632.2c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S537.9,632.2,536.6,632.2z M548.3,503.3c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S549.6,503.3,548.3,503.3z M501.4,515c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S502.7,515,501.4,515z M548.3,620.5c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S549.6,620.5,548.3,620.5z M548.3,632.2c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S549.6,632.2,548.3,632.2z M548.3,515c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S549.6,515,548.3,515z M524.9,561.9c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S526.2,561.9,524.9,561.9z M513.1,573.6c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S514.5,573.6,513.1,573.6z M513.1,561.9c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S514.5,561.9,513.1,561.9z M513.1,515c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S514.5,515,513.1,515z M513.1,632.2c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S514.5,632.2,513.1,632.2z M524.9,515c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S526.2,515,524.9,515z M501.4,503.3c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S502.7,503.3,501.4,503.3z M513.1,620.5c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S514.5,620.5,513.1,620.5z M524.9,573.6c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S526.2,573.6,524.9,573.6z M524.9,620.5c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S526.2,620.5,524.9,620.5z M524.9,632.2c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S526.2,632.2,524.9,632.2z M513.1,503.3c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S514.5,503.3,513.1,503.3z M786,561.9c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S787.3,561.9,786,561.9z M786,620.5c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S787.3,620.5,786,620.5z M786,550.1c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S787.3,550.1,786,550.1z M786,573.6c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S787.3,573.6,786,573.6z M786,585.3c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S787.3,585.3,786,585.3z M797.7,632.2c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S799,632.2,797.7,632.2z M786,608.7c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S787.3,608.7,786,608.7z M786,597c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S787.3,597,786,597z M797.7,538.4c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S799,538.4,797.7,538.4z M797.7,550.1c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S799,550.1,797.7,550.1z M797.7,561.9c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S799,561.9,797.7,561.9z M797.7,573.6c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S799,573.6,797.7,573.6z M809.4,632.2c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S810.7,632.2,809.4,632.2z M797.7,620.5c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S799,620.5,797.7,620.5z M797.7,608.7c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S799,608.7,797.7,608.7z M797.7,597c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S799,597,797.7,597z M797.7,585.3c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S799,585.3,797.7,585.3z M764.5,550.1c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S765.8,550.1,764.5,550.1z M717.6,608.7c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S718.9,608.7,717.6,608.7z M717.6,561.9c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S718.9,561.9,717.6,561.9z M717.6,585.3c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S718.9,585.3,717.6,585.3z M717.6,573.6c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S718.9,573.6,717.6,573.6z M717.6,597c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S718.9,597,717.6,597z M809.4,620.5c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S810.7,620.5,809.4,620.5z M717.6,620.5c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S718.9,620.5,717.6,620.5z M705.9,538.4c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S707.2,538.4,705.9,538.4z M717.6,632.2c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S718.9,632.2,717.6,632.2z M752.8,538.4c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S754.1,538.4,752.8,538.4z M752.8,550.1c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S754.1,550.1,752.8,550.1z M752.8,561.9c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S754.1,561.9,752.8,561.9z M717.6,550.1c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S718.9,550.1,717.6,550.1z M741.1,538.4c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S742.4,538.4,741.1,538.4z M764.5,561.9c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S765.8,561.9,764.5,561.9z M729.4,538.4c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S730.7,538.4,729.4,538.4z M741.1,550.1c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S742.4,550.1,741.1,550.1z M705.9,550.1c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S707.2,550.1,705.9,550.1z M741.1,561.9c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S742.4,561.9,741.1,561.9z M729.4,550.1c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S730.7,550.1,729.4,550.1z M844.6,585.3c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S845.9,585.3,844.6,585.3z M844.6,597c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S845.9,597,844.6,597z M844.6,573.6c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S845.9,573.6,844.6,573.6z M844.6,550.1c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S845.9,550.1,844.6,550.1z M844.6,561.9c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S845.9,561.9,844.6,561.9z M844.6,608.7c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S845.9,608.7,844.6,608.7z M844.6,632.2c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S845.9,632.2,844.6,632.2z M832.9,538.4c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S834.2,538.4,832.9,538.4z M844.6,620.5c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S845.9,620.5,844.6,620.5z M856.3,559.9c1.3,0,2.4-0.5,3.4-1.5
                                    s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4s-2.1-1.5-3.4-1.5c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4
                                    S855,559.9,856.3,559.9z M856.3,561.9c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S857.6,561.9,856.3,561.9z M856.3,573.6c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S857.6,573.6,856.3,573.6z M856.3,585.3c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S857.6,585.3,856.3,585.3z M844.6,538.4c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S845.9,538.4,844.6,538.4z M832.9,550.1c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S834.2,550.1,832.9,550.1z M856.3,620.5c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S857.6,620.5,856.3,620.5z M856.3,608.7c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S857.6,608.7,856.3,608.7z M856.3,597c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S857.6,597,856.3,597z M832.9,573.6c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S834.2,573.6,832.9,573.6z M809.4,550.1c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S810.7,550.1,809.4,550.1z M809.4,538.4c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S810.7,538.4,809.4,538.4z M821.1,620.5c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S822.5,620.5,821.1,620.5z M821.1,632.2c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S822.5,632.2,821.1,632.2z M809.4,561.9c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S810.7,561.9,809.4,561.9z M809.4,597c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S810.7,597,809.4,597z M809.4,573.6c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S810.7,573.6,809.4,573.6z M809.4,585.3c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S810.7,585.3,809.4,585.3z M832.9,608.7c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S834.2,608.7,832.9,608.7z M832.9,585.3c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S834.2,585.3,832.9,585.3z M821.1,550.1c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S822.5,550.1,821.1,550.1z M832.9,561.9c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S834.2,561.9,832.9,561.9z M809.4,608.7c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S810.7,608.7,809.4,608.7z M832.9,597c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S834.2,597,832.9,597z M821.1,538.4c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S822.5,538.4,821.1,538.4z M832.9,632.2c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S834.2,632.2,832.9,632.2z M832.9,620.5c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S834.2,620.5,832.9,620.5z M625.8,608.7c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S627.1,608.7,625.8,608.7z M625.8,632.2c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S627.1,632.2,625.8,632.2z M614.1,561.9c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S615.4,561.9,614.1,561.9z M614.1,503.3c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S615.4,503.3,614.1,503.3z M625.8,597c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S627.1,597,625.8,597z M614.1,515c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S615.4,515,614.1,515z M614.1,550.1c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S615.4,550.1,614.1,550.1z M625.8,585.3c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S627.1,585.3,625.8,585.3z M614.1,538.4c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S615.4,538.4,614.1,538.4z M614.1,526.7c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S615.4,526.7,614.1,526.7z M637.6,561.9c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S638.9,561.9,637.6,561.9z M625.8,503.3c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S627.1,503.3,625.8,503.3z M625.8,573.6c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S627.1,573.6,625.8,573.6z M625.8,526.7c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S627.1,526.7,625.8,526.7z M637.6,573.6c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S638.9,573.6,637.6,573.6z M625.8,515c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S627.1,515,625.8,515z M625.8,538.4c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S627.1,538.4,625.8,538.4z M625.8,561.9c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S627.1,561.9,625.8,561.9z M625.8,550.1c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S627.1,550.1,625.8,550.1z M625.8,620.5c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S627.1,620.5,625.8,620.5z M602.4,538.4c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S603.7,538.4,602.4,538.4z M602.4,573.6c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S603.7,573.6,602.4,573.6z M602.4,585.3c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S603.7,585.3,602.4,585.3z M602.4,550.1c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S603.7,550.1,602.4,550.1z M602.4,561.9c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S603.7,561.9,602.4,561.9z M602.4,620.5c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S603.7,620.5,602.4,620.5z M602.4,597c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S603.7,597,602.4,597z M602.4,632.2c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S603.7,632.2,602.4,632.2z M602.4,608.7c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S603.7,608.7,602.4,608.7z M602.4,503.3c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S603.7,503.3,602.4,503.3z M614.1,597c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S615.4,597,614.1,597z M614.1,608.7c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S615.4,608.7,614.1,608.7z M614.1,585.3c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S615.4,585.3,614.1,585.3z M614.1,620.5c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S615.4,620.5,614.1,620.5z M602.4,515c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S603.7,515,602.4,515z M614.1,632.2c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S615.4,632.2,614.1,632.2z M602.4,526.7c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S603.7,526.7,602.4,526.7z M614.1,573.6c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S615.4,573.6,614.1,573.6z M637.6,503.3c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S638.9,503.3,637.6,503.3z M694.2,597c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S695.5,597,694.2,597z M694.2,585.3c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S695.5,585.3,694.2,585.3z M694.2,608.7c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S695.5,608.7,694.2,608.7z M694.2,561.9c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S695.5,561.9,694.2,561.9z M694.2,573.6c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S695.5,573.6,694.2,573.6z M694.2,620.5c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S695.5,620.5,694.2,620.5z M637.6,515c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S638.9,515,637.6,515z M672.7,526.7c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S674,526.7,672.7,526.7z M694.2,632.2c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S695.5,632.2,694.2,632.2z M705.9,632.2c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S707.2,632.2,705.9,632.2z M705.9,585.3c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S707.2,585.3,705.9,585.3z M705.9,597c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S707.2,597,705.9,597z M705.9,573.6c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S707.2,573.6,705.9,573.6z M705.9,608.7c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S707.2,608.7,705.9,608.7z M672.7,538.4c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S674,538.4,672.7,538.4z M694.2,538.4c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S695.5,538.4,694.2,538.4z M694.2,550.1c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S695.5,550.1,694.2,550.1z M705.9,620.5c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S707.2,620.5,705.9,620.5z M672.7,515c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S674,515,672.7,515z M649.3,538.4c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S650.6,538.4,649.3,538.4z M649.3,526.7c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S650.6,526.7,649.3,526.7z M649.3,550.1c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S650.6,550.1,649.3,550.1z M649.3,503.3c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S650.6,503.3,649.3,503.3z M649.3,515c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S650.6,515,649.3,515z M705.9,561.9c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S707.2,561.9,705.9,561.9z M649.3,573.6c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S650.6,573.6,649.3,573.6z M649.3,561.9c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S650.6,561.9,649.3,561.9z M661,550.1c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S662.3,550.1,661,550.1z M661,503.3c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S662.3,503.3,661,503.3z M661,515c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S662.3,515,661,515z M672.7,561.9c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S674,561.9,672.7,561.9z M661,526.7c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S662.3,526.7,661,526.7z M672.7,550.1c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S674,550.1,672.7,550.1z M661,561.9c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S662.3,561.9,661,561.9z M661,573.6c-1.3,0-2.4,0.5-3.4,1.5
                                    s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4
                                    S662.3,573.6,661,573.6z M661,538.4c-1.3,0-2.4,0.5-3.4,1.5s-1.5,2.1-1.5,3.4c0,1.3,0.5,2.4,1.5,3.4s2.1,1.5,3.4,1.5
                                    c1.3,0,2.4-0.5,3.4-1.5s1.5-2.1,1.5-3.4c0-1.3-0.5-2.4-1.5-3.4S662.3,538.4,661,538.4z"/>
                                    </svg>')
                      ))
      ),
      fluidRow(class = "myRow3",
               plotOutput("slice_plots", height = "900px",
                          hover = hoverOpts(id ="plot_hover"))
      ),
      width = 12)
  ))

  ######################################################################################################
  ######################################### Run Shiny App  #############################################
  ######################################################################################################
  adepro_app <- shinyApp(ui = ui, server = server)
  if (!is.null(browser)) options(browser = browser)
  shiny::runApp(adepro_app, host = host, port = port)
}
