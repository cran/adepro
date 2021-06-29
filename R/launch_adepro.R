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
#'
#'
#' ## Running AdEPro on test data
#' ## ---------------------------
#' write.csv(example_aedata, file=paste(tempdir(), "\\example_aedata.csv", sep=""), row.names=FALSE)
#' write.csv(example_sldata, file=paste(tempdir(), "\\example_sldata.csv", sep=""), row.names=FALSE)
#' ## Load example_aedata.csv (and example_sldata.csv) in the Upload data panel after
#' ## launching the application.
#' }
#'
#' @importFrom graphics barplot legend symbols matplot text polygon par rect
#' @import utils
#' @import shiny
#' @import V8
#' @importFrom shinyBS updateCollapse bsCollapse bsCollapsePanel
#' @importFrom shinyWidgets knobInput pickerInput circleButton radioGroupButtons
#' @importFrom shinyjs useShinyjs extendShinyjs inlineCSS click delay disable enable reset
#' @importFrom MASS eqscplot
#' @importFrom audio play audioSample
#' @importFrom TeachingDemos my.symbols
#' @importFrom shape plotcircle
#' @importFrom seriation seriate get_order
#' @import gclus
#' @import Cairo
#' @importFrom dplyr %>% filter pull mutate select arrange n left_join right_join group_by ungroup rename rowwise
#' @importFrom readr read_csv
#' @importFrom rlang sym
#' @importFrom tidyr nest drop_na gather spread
#' @importFrom haven read_sas
#' @importFrom stats aggregate na.omit
#' @return A shiny app
#'

launch_adepro <- function(host = "127.0.0.1", port = NULL, browser = NULL) {
  #### Global ####
  day_end <- day_start <- r <- ps <- treat <- NULL
  colors <- c("#e43157", "#377eb8", "#4daf4a", "#984ea3",
             "#ff7f00", "#ffff33", "#a65628", "#f781bf")
  options(shiny.usecairo = TRUE)
  options(shiny.maxRequestSize = 60*1024^2)
  poly_f <- function(num, rad = 1, fg = par('fg'), bg = par('fg'), ...) {
    x_tmp <- c(0, 0 + rad * 0.9 * cos(seq(pi / 2 - 2 * pi / 8 * (num - 1), pi / 2 - 2 * pi / 8 * num, length = 10)))
    y_tmp <- c(0, 0 + rad * 0.9 * sin(seq(pi / 2 - 2 * pi / 8 * (num - 1), pi / 2 - 2 * pi / 8 * num, length = 10)))
    polygon(c(x_tmp, x_tmp[1]), c(y_tmp, y_tmp[1]), col = bg, border = fg, ...)
    NULL
  }
  shiny::addResourcePath("sbs", system.file("www", package = "shinyBS"))
  #### Server ####
  server <- shiny::shinyServer(function(input, output, session) {
  #### UI ELEMENTS ####
  #### Top panel ####
    output$speed <- shiny::renderUI({
      shinyWidgets::knobInput(inputId = "speed",
                              label = shiny::HTML('<p style="color:white"> Animation Speed (sec.)</p>'),
                              value = 2,
                              min = 1,
                              max = 10,
                              step = 1,
                              inputColor = "white",
                              fgColor = "#377EB8",
                              width = "90px",
                              height = "90px",
                              cursor = TRUE
      )
    })

    output$dynamic_css <- shiny::renderUI({
      shiny::req(input$slider)
      shiny::tags$style(type = "text/css","#dayinfo { overflow-y: hidden; overflow-x: hidden }",
                        paste0("#dayinfo {background-color: #E43157; max-height: 100px; width: ", 20 + (35 * nchar(input$slider)), "px; color: #ffffff; text-align = justify; border-radius: 10px; border-color: #383838; font: Arial; font-size: 60px;}"
                        )
      )
    })

    output$slider <- shiny::renderUI({
      if (is.null(ae_data()) | is.null(patient_data())) {
        return(NULL)
      }
      ae_data <- ae_data()
      day_max <- ifelse(length(ae_data$day_end) == 0,
                        1,
                        max(ae_data$day_end)
      )
      uiElement <- shiny::sliderInput(inputId = "slider",
                                      label = NULL,
                                      min = 1,
                                      max = day_max,
                                      value = 1, step = 1,
                                      width = '100%'
      )
      return(uiElement)
    })

    output$legend <- shiny::renderPlot({
      if (is.null(ae_data()) | is.null(patient_data())) {
        return(NULL)
      } else {
        return(pie_legend(aes = input$var))
      }
    }, bg = "#383838")

    output$dayinfo <- shiny::renderText({
      if (is.null(ae_data()) & is.null(patient_data())) {
        return(cat(""))
      } else {
        input$slider
      }
    })

    #### Upload data ####
    output$tot_dat <- shiny::renderUI({
      shiny::fileInput(inputId = 'tot_dat',
                       label = 'Upload Adverse Event Data')
    })

    output$tot_dat2 <- shiny::renderUI({
      shiny::fileInput(inputId = 'tot_dat2',
                       label = 'Upload Subject Level Data')
    })

    output$file_upload <- shiny::renderUI({
      uiElement <- list(
        shiny::helpText(shiny::HTML(paste0('<p style="color:white"> Please upload your <span style = "color:#E43157">*.sas7bdat </span> or <span style = "color:#E43157">*.csv </span> file(s). </p>'))),
        shiny::uiOutput('tot_dat')
      )
      return(uiElement)
    })

    output$refresh <- shiny::renderUI({
      shiny::actionButton(inputId ="refresh",
                          label = "Refresh Data",
                          icon = icon("refresh"),
                          style = paste0("color:#FFFFFF ; background-color: #377EB8;")
      )
    })

    shiny::observeEvent(input$refresh, {
      loaded$dat <- 0
    })

    output$heightSlider <- shiny::renderUI({
      shiny::sliderInput(inputId = "heightSlider",
                         label = "Choose Plot height (in pixel)",
                         value = 800,
                         min =  400,
                         max = 1600,
                         step = 100
      )
    })

    output$numberRows <- shiny::renderUI({
      shiny::req(patient_data())
      patient <- patient_data()
      value <- ceiling(sqrt(850 / 1920 * nrow(patient)))
      shiny::sliderInput(inputId = "numberRows",
                         label = "Number of Rows",
                         min = 1,
                         max = 2 * value,
                         value = value,
                         step = 1
      )
    })

    #### Modify data ####
    #### AI ####
    output$varSeq <- shiny::renderUI({
      shiny::req(all_aes())
      choices <- all_aes()
      choices <- c(choices,
                   paste(choices, "sev1", sep = "_"),
                   paste(choices, "sev2", sep = "_"),
                   paste(choices, "sev3", sep = "_")
      )
      most_freq_aes <- head(choices, 8)
      shinyWidgets::pickerInput(inputId = 'varSeq',
                                label = 'Sequencing input',
                                choices,
                                multiple = TRUE,
                                selected = most_freq_aes,
                                options = list(`actions-box` = TRUE,
                                               `selected-text-format` = 'count > 0',
                                               `count-selected-text` = '{0} selected (of {1})',
                                               `live-search`=TRUE,
                                               `style`='background: btn-primary',
                                               `header`='Select multiple items',
                                               `none-selected-text`='All dropped!'
                                )
      )
    })

    output$ae_type <- shiny::renderUI({
      if (is.null(ae_data()) | is.null(patient_data())) {
        return(NULL)
      }
      global_params <- shiny::isolate(global_params())
      shiny::selectizeInput(inputId = "type",
                             label = "Select type of Adverse Event:",
                             choices  = global_params$AE_options,
                             selected = 1
      )
    })

    output$ae_sorting <- shiny::renderUI({
      if (is.null(ae_data()) | is.null(patient_data())) {
        return(NULL)
      }
      var_sort <- colnames(patient_data())[-c(1:4)]
      shiny::selectInput(inputId = "sorting",
                         label = "Sort patients by:",
                         choices = c(var_sort),
                         selected = "SUBJIDN"
      )
    })

    AI.AdePro_reac <- shiny::reactiveValues(val = 1)

    shiny::observeEvent(c(input$AI.Update, patient_data()), {
      AI.AdePro_reac$val <- AI.AdePro_reac$val + 1
    })

    #### Adverse Events for animation ####
    output$ae_var <- shiny::renderUI({
      if (is.null(ae_data()) | is.null(patient_data())) {
        return(NULL)
      }
      most_freq_aes <- head(all_aes(), 8)
      rare_ae <- all_aes()[length(all_aes())]
      trt <- levels(factor(patient_data()$treat))
      shiny::selectizeInput(inputId = "var",
                            label = "Choose Adverse Events for display (max. 8):",
                            options  = list(maxItems = 8),
                            choices  = all_aes(),
                            selected = most_freq_aes,
                            multiple = TRUE
      )
    })

    output$ae_audio <- shiny::renderUI({
      if (is.null(ae_data()) | is.null(patient_data())) {
        return(NULL)
      }
      rare_ae <- all_aes()[length(all_aes())]
      shiny::selectInput(inputId = "audio",
                         label = "Choose Adverse Event for audio:",
                         choices = all_aes(),
                         selected = rare_ae
      )
    })

    output$ae_sound1 <- shiny::renderUI({
      if (is.null(ae_data()) | is.null(patient_data())) {
        return(NULL)
      }
      trt <- levels(factor(patient_data()$treat))
      shiny::selectInput(inputId = "sound1",
                         label = "Choose Treatment Group for first sound:",
                         choices = c("- none -", trt),
                         selected = "- none -"
      )
    })

    output$ae_sound2 <- shiny::renderUI({
      if (is.null(ae_data()) | is.null(patient_data())) {
        return(NULL)
      }
      trt <- levels(factor(patient_data()$treat))
      shiny::selectInput(inputId = "sound2",
                         label = "Choose Treatment Group for second sound:",
                         choices = c("- none -", trt),
                         selected = "- none -"
      )
    })

    #### Piecharts ####
    output$slicePlots <- shiny::renderPlot({
      total_data_reac()
      if (is.null(total_data_reac()) | is.null(ae_data()) | is.null(patient_data())) {
        return(NULL)
      }
      session$clientData$output_slicePlots_width
      input$add_row
      input$rem_row
      input$heightSlider
      input$plus_zoom
      input$minus_zoom
      input$type
      req(input$type)
      req(data_type())
      req(count_max())
      patients <- patients()
      global_params <- global_params()
      height <- global_params$height
      width  <- global_params$width
      xlines <- global_params$xlines
      ylines <- global_params$ylines
      plines <- global_params$plines[[1]]
      xval <- c(0, cumsum(plines)[-length(plines)]) + plines/2
      title <- as.character(unique(patients$treat))

      on_ex <- par("oma","mar","plt")
      on.exit(par(on_ex))
      par(oma = c(0, 0, 0, 0),
          mar = c(0, 0, 0, 0),
          plt = c(0.05, 0.99, 0.01, 0.99)
      )
      if (!is.null(patients$X) & !is.null(patients$Y)) {
      MASS::eqscplot(patients$X,
                     patients$Y,
                     tol = 0,
                     axes = FALSE,
                     xlab = "",
                     ylab = "",
                     main = "",
                     xlim = c(0, max(patients$X) + 1),
                     ylim = c(min(patients$Y) - 1, 2),
                     type = "n"
      )

      cont <- ifelse(input$slider > patients$end, "#424242", "#383838")
      cont <- ifelse(input$slider >= patients$death, "black", cont)
      cont_bg <- ifelse(input$slider >= patients$death, "black","#383838")

      graphics::symbols(patients$X,
                        patients$Y,
                        circles = cbind(rep(0.85, length(patients$Y))),
                        inches = FALSE,
                        add = TRUE,
                        fg = cont_bg,
                        bg = cont,
                        lwd = 3,
                        xlab = "",
                        ylab = "",
                        main = "",
                        xlim = c(0, 2 * sum(width)),
                        ylim = c(-2 * height, 3)
      )
      graphics::matplot(xlines[[1]],
                        ylines[[1]],
                        type = "l",
                        lty = 2,
                        col = "#6b6b6b",
                        add = TRUE
      )
      graphics::text(x = xval,
                     y = rep(1, length(xval)),
                     labels = title,
                     col = "#6b6b6b",
                     cex = 1.5
      )
      day <- input$slider
      if (length(input$var) > 0) {

        for (i in 1:length(input$var)) {
          for (j in c(0.5, 0.75, 1)) {
            tmp <- data() %>%
              dplyr::filter(ae == input$var[i], day_end < day, r == j) %>%
              dplyr::select(patient)
            tmp1 <- data() %>%
              dplyr::filter(ae == input$var[i], day_end >= day, day_start <= day, r == j) %>%
              dplyr::select(patient)
            tmp2 <- patients %>%
              dplyr::filter(ps %in% tmp$patient & !(ps %in% tmp1$patient))
            TeachingDemos::my.symbols(x = tmp2$X,
                                      y = tmp2$Y,
                                      symb = poly_f,
                                      num = i,
                                      rad = j,
                                      bg = cont[which(patients$ps %in% tmp2$ps & !(ps %in% tmp1$patient))],
                                      fg = colors[i],
                                      xsize = 2,
                                      add = TRUE
            )
            tmp <- data() %>%
              dplyr::filter(ae == input$var[i], day_end >= day, day_start <= day, r == j) %>%
              dplyr::select(patient)
            tmp2 <- patients %>%
              dplyr::filter(ps %in% tmp$patient)
            TeachingDemos::my.symbols(x = tmp2$X,
                                      y = tmp2$Y,
                                      symb = poly_f,
                                      num = i,
                                      rad = j,
                                      bg = colors[i],
                                      fg = colors[i],
                                      xsize = 2,
                                      add = TRUE
            )
          }
        }
        counts <- counts()
        tf1 <- max(c(0, counts$freq[which(counts$day == input$slider & counts$treat == input$sound1)]))
        tf2 <- max(c(0, counts$freq[which(counts$day == input$slider & counts$treat == input$sound2)]))
        tone(tf1, tf2)
      }
      }
      }, bg = "#424242", height = function(){ heightSlider$val }, width = function() {
      session$clientData$output_slicePlots_width
      }
    )
    #### Barplot ####
    output$barchart <- shiny::renderPlot({
      session$clientData$output_barchart_width
      input$add_row
      input$rem_row
      input$type
      input$heightSlider
      input$plus_zoom
      input$minus_zoom
      #shiny::req(input$type)
      shiny::req(input$var)
      shiny::req(data_type())
      shiny::req(count_max())
      shiny::req(patient_data())
      shiny::req(all_aes())
      ae <- data_type()
      patient <- patient_data()
      day_slider <- input$slider
      vari <- input$var
      day_mx <- ifelse(length(ae$day_end) == 0,
                       1,
                       max(ae$day_end)
      )
      count_mx <- count_max()
        bar_chart(ae_data = ae,
                  patients = patient,
                  day = day_slider,
                  variables = vari,
                  day_max = day_mx,
                  count_max = count_mx,
                  cex.n = (((2.5 - 1.5) * heightSlider$val + (1.5 * 1600 - 3 * 400)) / (1600 - 400))
        )


      }, bg = "#424242", height = function(){heightSlider$val }, width = function() {
      session$clientData$output_barchart_width}
    )
      #### Absolute Panel ####
      #### Number of Rows ####
      output$rowpanel <- shiny::renderUI({
        shiny::absolutePanel(
          id = "controls",
          class = "modal-content",
          fixed = TRUE,
          draggable = TRUE,
          top = 10,
          left = "auto",
          right = 50,
          bottom = "auto",
          width = 75,
          height = 75,
          shiny::HTML('<p style="color:white"> Number of Rows: </p>'),
          shiny::fluidRow(
            shiny::column(2,
                          div(style = "display:inline-block", shinyWidgets::circleButton(inputId = "rem_row",
                                                                                         icon = icon("minus"),
                                                                                         size = "xs",
                                                                                         status = "primary"))),
            shiny::column(2,
                          div(style = "display:inline-block", shinyWidgets::circleButton(inputId = "add_row",
                                                                                         icon = icon("plus"),
                                                                                         size = "xs",
                                                                                         status = "primary")))
          )
        )
      })
      #### Height ####
      output$zoompanel <- shiny::renderUI({
        shiny::absolutePanel(
          id = "controls",
          class = "modal-content",
          fixed = TRUE,
          draggable = TRUE,
          top = 90,
          left = "auto",
          right = 50,
          bottom = "auto",
          width = 75,
          height = 75,
          shiny::HTML('<p style="color:white"> Change Plot height: </p>'),
          shiny::fluidRow(
            shiny::column(2,
                          div(style = "display:inline-block", shinyWidgets::circleButton(inputId = "minus_zoom",
                                                                                         icon = icon("minus"),
                                                                                         size = "xs",
                                                                                         status = "primary"))),
            shiny::column(2,
                          div(style = "display:inline-block", shinyWidgets::circleButton(inputId = "plus_zoom",
                                                                                         icon = icon("plus"),
                                                                                         size = "xs",
                                                                                         status = "primary")))
          )
        )
      })

    #### Patient Information ####
    output$overall_info <- shiny::renderPrint({
      if (is.null(ae_data()) | is.null(patient_data())) {
        return(cat(""))
      }
      patients <- patients()
      global_params <- global_params()
      footnote <- global_params$footnote
      # proportion of TEAEs that have already started
      prop <- round(mean(ae_data()$day_start <= input$slider) * 100, 0)
      txt <- footnote
      cat(txt, paste(prop, "% of TEAEs have \nalready started", sep = ""), sep = "\n")
    })


    hoverinf <- shiny::reactiveValues(val = NULL)

    shiny::observeEvent(input$plot_hover, {
      if (!is.null(input$plot_hover)) {
        hoverinf$val <- input$plot_hover
      }
    })

    output$plot_hoverinfo <- shiny::renderPrint({
      shiny::req(total_data_reac())
      if (is.null(ae_data()) | is.null(patient_data())) {
        return(cat(""))
      }
      patients <- patients()
      global_params <- global_params()
      footnote <- global_params$footnote
      # proportion of TEAEs that have already started
      prop <- round(mean(ae_data()$day_start <= input$slider) * 100, 0)

      info <- shiny::nearPoints(patients, hoverinf$val,
                                threshold = 30, maxpoints = 1,
                                xvar = "X", yvar = "Y")

      if(nrow(info) > 0) {
        info <- info[-c(2:4, length(info), length(info) - 1)]
        pr <- paste("Subject ID:", info[1])
        if (ncol(info) != 2) {
          pr2 <- sapply(2:(ncol(info) - 1), function(x) paste(colnames(info)[x], ": ", info[, x], sep = ""))
          for (i in 1:length(pr2)) {
            pr <- paste(pr, pr2[i], sep = "\n")
          }
        }
        txt <- pr
      } else {
        txt <- "(hover over circle)"
      }
      cat(txt, sep = "\n")
    })

    output$patientpanel <- shiny::renderUI({
      shiny::absolutePanel(
        id = "controls",
        class = "modal-content",
        fixed = TRUE,
        draggable = TRUE,
        shiny::HTML(paste0("<button style = 'background: #424242;
                                             color:#ffffff',
                           data-toggle = 'collapse' data-target = '#demo3'>
                           <i class='fa fa-compress'></i> Info Box</button>")),
        top = 180,
        left = "auto",
        right = 50,
        bottom = "auto",
        width = 250,
        height = "auto",
        tags$div(id = 'demo3',  class = "collapse",
           shiny::HTML('<p style="color:white"> Overall information: </p>'),
           shiny::fluidRow(
            shiny::column(12,
              shiny::verbatimTextOutput("overall_info")
            )
           ),
           shiny::HTML('<p style="color:white"> Patient information: </p>'),
           shiny::fluidRow(
            shiny::column(12,
              shiny::verbatimTextOutput("plot_hoverinfo")
            )
           )
        )
      )
    })
    #### REACTIVE OBJECTS ####
    #initialize timer with 1 second
    adepro_timer <- shiny::reactiveTimer(1000)
    adepro_n <- 10

    onoff <- shiny::reactiveValues(val = FALSE)

    my <- shiny::reactiveValues(inc = 0,
                                started = FALSE)

    loaded <- shiny::reactiveValues(dat = 0)
    output$load <- shiny::reactive(loaded$dat)
    shiny::outputOptions(output, "load", suspendWhenHidden = FALSE)


    start <- shiny::reactiveValues(dat = 0)
    output$flag <- shiny::reactive(start$dat)
    shiny::outputOptions(output, "flag", suspendWhenHidden = FALSE)

    ae_input <- shiny::reactiveValues(val = NULL)
    patient_input <- shiny::reactiveValues(val = NULL)


    heightSlider <- shiny::reactiveValues(val = 800)

    infile_adae <- shiny::reactiveValues(val = NULL)
    infile_adsl <- shiny::reactiveValues(val = NULL)

    shiny::observeEvent(input$tot_dat, {
      infile_adae$val <- input$tot_dat$datapath
    })

    shiny::observeEvent(input$tot_dat2, {
      infile_adsl$val <- input$tot_dat2$datapath
    })

    total_data_reac <- shiny::reactive({
      inFile <- infile_adae$val
      if (is.null(inFile)) {
        return(NULL)
      } else {
        #adae_path <- infile_adae$val
        split_path <- strsplit(x = inFile, split = "[.]")
        path_ending <- split_path[[1]][length(split_path[[1]])]
      if (path_ending %in% c("csv", "sas7bdat", "sas7cdat")) {
        if (path_ending == "csv") {
          adae <- suppressWarnings(readr::read_csv(inFile, col_types = readr::cols("SEX" = "c"), na = c(".", "NA")))
          colnames(adae) <- toupper(colnames(adae))
          adae[adae == "."] <- NA
        } else if (path_ending == "sas7bdat" | path_ending == "sas7cdat") {
        adae <- haven::read_sas(inFile)
        adae <- adae %>%
          dplyr::select(stats::setNames(colnames(adae), toupper(colnames(adae))))
        }
        inFile2 <- infile_adsl$val
        if (is.null(inFile2)) {
          adsl <- NULL
        } else {
          adsl_path <- inFile2
          split_path <- strsplit(x = adsl_path, split = "[.]")
          path_ending <- split_path[[1]][length(split_path[[1]])]
          if (path_ending %in% c("csv", "sas7bdat", "sas7cdat")) {
            if (path_ending == "csv") {
              adsl <- suppressWarnings(readr::read_csv(inFile2, col_types = readr::cols("SEX" = "c"), na = c(".", "NA")))
              colnames(adsl) <- toupper(colnames(adsl))
              adsl[adsl == "."] <- NA
            } else if (path_ending == "sas7bdat" | path_ending == "sas7cdat") {
              adsl <- haven::read_sas(inFile2)
              adsl <- adsl %>%
                dplyr::select(stats::setNames(colnames(adsl), toupper(colnames(adsl))))
            }
          }
        }


        data <- prepare_data(dat = adae, adsl_data = adsl)

        if (!is.null(data$ae_data) &
            dim(data$ae_data)[1] != 0 &
            dim(data$pat_data)[1] != 0 &
            !is.null(data$pat_data) &
            all(dim(data$ae_data) != c(1, 1)) &
            all(dim(data$pat_data) != c(1, 1))) {
          data
        } else {
          if(data$ae_data == "Treatment variable has different properties in ADAE and ADSL data sets. Please check the AdEPro package manual.") {
            loaded$dat <- 3
          } else {
            loaded$dat <- 2
          }
        return(NULL)
        }
      } else {
          loaded$dat <- 2
        return(NULL)
      }

      }
    })

    ae_data <- shiny::reactive({
      shiny::req(total_data_reac())
      total_data_reac()
      tmp <- total_data_reac()$ae_data
      tmp
    })

    patient_data <- shiny::reactive({
      shiny::req(total_data_reac())
      tmp <- total_data_reac()$pat_data
      tmp
      ae_data <- ae_data()
      # add column in patient_data: ae_frequency - AE frequency
      tmp$ae_frequency <- numeric(nrow(tmp))
      for (i in 1:nrow(tmp)) {
        indices <- which(ae_data$patient == tmp$ps[i])
        tmp$ae_frequency[i] <- sum(ae_data$day_end[indices] - ae_data$day_start[indices] + rep(1, length(indices)))
      }
      tmp
    })

    has_data <- shiny::reactive({
      return(!(is.null(ae_data()) & is.null(patient_data())))
    })

    data_type <- shiny::reactive({
      shiny::req(input$type)
      ae_data0 <- ae_data()
      Q <- initQ(ae_data0)
      ae_data <- ae_data0[which(Q[,as.numeric(input$type)]),]
      ae_data
    })

    data_raw <- shiny::reactive({
      total_data_reac()
      ae_data <- ae_data()
      Q <- initQ(ae_data)
      ae_data <- preproc_ae(ae_data)
      ae_data <- ae_data[which(Q[, as.numeric(input$type)]), ]
      ae_data
    })

    data <- shiny::reactive({
      data <- data_raw()
      selected <- data_raw()$ae %in% input$var
      data <- data[selected, ]
      data
    })

    all_aes <- shiny::reactive({
      dat <- data_raw()
      ae_table <- sort(table(rep(dat$ae, dat$day_end - dat$day_start + 1)), decreasing = TRUE)
      ae_table <- ae_table[ae_table > 0]
      aes <- names(ae_table)
      aes
    })

    seq_matrix <- shiny::eventReactive(c(input$AI.AdEPro, input$AI.Update, input$remove_adsl), {
      shiny::req(ae_data())
      shiny::req(patient_data())
      shiny::req(input$varSeq)
      ae_data <- ae_data()
      patient_d <- patient_data()
      tmp <- order_patient(ae_data = ae_data,
                           patients = patient_d,
                           variables = input$varSeq,
                           method_dist = 'euclidean',
                           method_seriate = input$methSeq)
      tmp
    })

    #Count the Maximal Number of (selected) Adverse Events per Treatment per day for the y-Axis of the Barplots
    count_max <- shiny::reactive({
      shiny::req(data_type(), patient_data(), input$var)
      ae <- data_type()
      patient <- patient_data()
      all_aes <- shiny::isolate(input$var)

       if(dim(ae %>%
                   dplyr::filter(ae %in% all_aes))[1] > 0) {
        tst <- ae %>%
          dplyr::filter(ae %in% all_aes) %>%
          dplyr::right_join((patient %>%
                               dplyr::rename(patient = ps)), by = "patient")
        tst <- tst%>%
          dplyr::select(day_start, day_end, treat, ae) %>%
          stats::na.omit()

        tst <- tst%>%
          dplyr::group_by(ae, treat) %>%
          tidyr::nest()
        max_count <- numeric(dim(tst)[1])
        for (i in 1:dim(tst)[1]) {
          max_count[i] <- apply(tst$data[[i]], 1, function(x){x[1]:x[2]}) %>%
            unlist() %>%
            table() %>%
            max()
        }
        count_max <- max(max_count)
      count_max
      }
    })

    counts <- shiny::reactive({
      shiny::req(ae_data(),patient_data(), input$audio, total_data_reac())
      ae_data <- ae_data()
      patient_data <- patient_data()
      dat <- ae_data[which(ae_data$ae == input$audio), ]
      if(dim(dat)[1] > 0) {
        data <- ae_count(dat, patient_data)
      } else {
        data <- NULL
      }
      return(data)
    })

    global_params <- shiny::reactive({
      title <- as.character(unique(patient_data()$treat))
      globals <- set_global_params(ae_data(), patient_data(), title = title, height = input$numberRows)
      return(globals)
    })

    patients <- shiny::reactive({
      total_data_reac()
      global_params <- shiny::req(global_params())
      patient_data  <- shiny::req(patient_data())
      if (!is.null(dim(patient_data)[1])) {
      if (!is.null(input$sorting) && input$sorting != "randomization number" && input$sorting != "SEQUENCING") {

        colindex <- which(colnames(patient_data) == input$sorting)

        trt <- as.numeric(factor(patient_data$treat, levels = unique(patient_data$treat)))

        if (is.null(dim(patient_data[, colindex]))){
        patient_data <- patient_data[order(trt, patient_data[, colindex]), ]
        patient_data <- patient_data %>%
          dplyr::arrange(treat)
        } else {
          NULL
        }
      } else if (!is.null(input$sorting) && input$sorting != "randomization number" && input$sorting == "SEQUENCING") {
        patient_data <- patient_data %>%
          dplyr::right_join(seq_matrix() %>%
                              dplyr::rename(ps = patient), by = 'ps')
        patient_data <- patient_data %>%
          dplyr::arrange(treat)
      }


      patient_data  <- preproc_patients(patient_data, global_params$height)
      patient_data
      } else {
        NULL
      }
    })


    #### OBSERVERS ####
    shiny::observeEvent(input$remove_adsl, {
      shinyjs::reset('tot_dat2')
      infile_adsl$val <- NULL
    })

    shiny::observeEvent(input$play, {
      my$started <- TRUE
    })

    shiny::observeEvent(input$pause, {
      my$started <- FALSE
    })

    shiny::observeEvent(input$play, {
      onoff$val <- TRUE
    })

    shiny::observeEvent(c(input$speed, input$play), {
      if(!is.null(input$speed)){
        adepro_n <<- shiny::isolate(input$speed)
        adepro_timer <<- shiny::reactiveTimer(adepro_n * 1000)
      }
    })

    shiny::observeEvent(input$pause,{
      onoff$val <- FALSE
    })

    shiny::observeEvent(input$slider, {
      my$inc <- shiny::isolate(input$slider)
    })

    shiny::observe({
      adepro_timer()
      if(shiny::isolate(my$started))
        my$inc <- shiny::isolate(my$inc) + 1
      if(onoff$val){
        shiny::updateSliderInput(session, inputId = "slider", value = isolate(my$inc))
      }
    })


    shiny::observeEvent(input$ae_dat, {
      ae_input$val <- input$ae_dat
    })

    shiny::observeEvent(input$patient_dat, {
      patient_input$val <- input$patient_dat
    })


    shiny::observeEvent(input$refresh, {
      patient_input$val <- NULL
      ae_input$val <- NULL
    })

    shiny::observeEvent(loaded$dat == 1, {
      shinyjs::disable("patient_dat")
      shinyjs::disable("ae_dat")
    })

    shiny::observe({
      shinyBS::updateCollapse(session, id = "collapse",
                              open = shiny::HTML('<p style="color:white; font-size:100%;"> Upload data </p>'))
    })

    shiny::observeEvent(has_data(), {
      shinyBS::updateCollapse(session, id = "collapse",
                              open = c(shiny::HTML('<p style="color:white; font-size:100%;"> Modify data </p>'),
                                       shiny::HTML('<p style="color:white; font-size:100%;"> Adverse Events for animation </p>')))
    })

    shiny::observeEvent(input$type, {
                          shinyBS::updateCollapse(session, id = "collapse",
                                                  open = shiny::HTML('<p style="color:white; font-size:100%;"> Adverse Events for animation </p>'))
    })

    shiny::observeEvent(input$rem_row, {
      shiny::updateSliderInput(session, inputId = "numberRows", value = (input$numberRows - 1))
    })

    shiny::observeEvent(input$add_row, {
      shiny::updateSliderInput(session, inputId = "numberRows", value = (input$numberRows + 1))
    })

    shiny::observeEvent(input$minus_zoom, {
      if (heightSlider$val > 400) {
       updateSliderInput(session, inputId = "heightSlider", value = input$heightSlider - 100)
      }
    })

    shiny::observeEvent(input$plus_zoom, {
      if (heightSlider$val < 1600) {
        updateSliderInput(session, inputId = "heightSlider", value = input$heightSlider + 100)
      }
    })

    shiny::observeEvent(heightSlider$val, {
      if (heightSlider$val == 400) {
        shinyjs::disable("minus_zoom")
      } else if (heightSlider$val > 400) {
        shinyjs::enable("minus_zoom")
      }
      if (heightSlider$val == 1600) {
        shinyjs::disable("plus_zoom")
      } else if(heightSlider$val < 1600) {
        shinyjs::enable("plus_zoom")
      }
    })

    shiny::observeEvent(input$heightSlider, {
      heightSlider$val <- input$heightSlider
    })

    shiny::observeEvent(has_data(), {
      if (!(is.null(ae_data())) & !is.null(patient_data())) {
        loaded$dat <- 1
      }
    })

    shiny::observeEvent(input$AI.AdEPro, {
      start$dat <- input$AI.AdEPro %% 2
    })

    shiny::observeEvent(input$AI.Update, {
      var_sort <- colnames(patient_data())[-c(1:4)]
      var_sort <- c("SEQUENCING", var_sort)
      shiny::updateSelectInput(session, "sorting",choices = var_sort, selected = "SEQUENCING")
    })

  })

  #### User Interface (UI) ####

  ui <- shiny::shinyUI(
    shiny::fluidPage(
    ## CSS codes:
    shiny::tags$head(shiny::tags$style(shiny::HTML('#controls {
                                          background-color: #383838;}'
                                       )
                      )
    ),
    shiny::tags$head(
      shiny::tags$style(shiny::HTML(".shiny-output-error-validation {
                                      color: #e43157;
                                      }"
                                    )
                        )
    ),
    shinyjs::useShinyjs(),
    tags$head(
      tags$style(
        HTML(
          "
          .form-group {
          margin-bottom: 0 !important;
          }
          "
        )
      )
    ),
    shiny::tags$style(type = "text/css", "body { overflow-y: scroll; }",
                      paste0(".recalculating {opacity: 1.0;}
                              .irs-bar {width: 100%; height: 25px; background: #377eb8; border-top: 1px solid #377eb8; border-bottom: 1px solid #377eb8;}
                              .irs-bar-edge {background: #377eb8; border: 1px solid #377eb8; height: 25px; border-radius: 5px; width: 20px;}
                              .irs-line {border: 1px solid #377eb8; height: 25px; border-radius: 5px;}
                              #numberRows .irs-grid-text {display: none;}
                              #heightSlider .irs-grid-text {display: none;}
                              #slider .irs-grid-text {font-family: 'arial'; color: black; bottom: 17px; z-index: 1;font-size:15px}
                              .irs-grid-pol {display: none;}
                              .irs-max {font-family: 'arial'; color: #ffffff; height:15px; font-size:15px}
                              .irs-min {font-family: 'arial'; color: #ffffff; height:15px; font-size:15px}
                              #numberRows .irs-single {color:#ffffff; height:20px; font-size:15px;}
                              #heightSlider .irs-single {color:#ffffff; height:20px; font-size:15px;}
                              #slider .irs-single {color:#ffffff; background:#e43157; height:20px; font-size:15px;}
                              .irs-slider {width: 30px; height: 30px; top: 22px;}
                              .panel-group.sbs-panel-group{position: absolute;width: 90%;}
                              .panel.panel-default{background-color:#383838; color:#ffffff;border-color:#6b6b6b}
                              .panel-heading{color:#ffffff;padding:0;}
                              .panel-title{background:#383838; color:#ffffff;margin-top:10px;margin-bottom:10px;padding-left:5px}
                              body {background-color: #424242; color: #6b6b6b}
                              .panel-group.sbs-panel-group {position: absolute; width: 100%;}
                              .panel.panel-default {background-color: #383838; color: #ffffff; border-color: #6b6b6b}
                              .panel-heading {color: #ffffff; padding: 0;}
                              .panel-title {background: #383838;color: #ffffff; margin-top: 10px; margin-bottom: 10px; padding-left: 5px}
                              .myRow1 {background-color: #383838; height: 170px;}
                              .myRow3 {background-color: #424242;}
                              #plot_hoverinfo {background-color: #424242; color: #ffffff; border-color: #383838; font-size: 14px;}
                              #overall_info{background-color: #424242; color: #ffffff; border-color: #383838; font-size: 14px;}
                              "
                      )
    ),
    # Main panel
    shiny::mainPanel(
      shiny::fluidRow(class = "myRow1",
        shiny::column(2,
          shiny::br(),
          shinyBS::bsCollapse(
            shinyBS::bsCollapsePanel(
              shiny::HTML('<p style="color:white; font-size:100%;"> Upload data </p>'),
              shiny::uiOutput("file_upload"),
              shinyWidgets::prettyToggle(inputId = 'showPanel1',
                                        label_off = 'Add ADSL File Input',
                                        label_on = 'Hide ADSL File Input',
                                        value = FALSE,
                                        outline = TRUE,
                                        status_on = "default",
                                        status_off = "default",
                                        plain = TRUE,
                                        icon_off = icon("plus"),
                                        icon_on = icon ("minus")
              ),
              shiny::conditionalPanel(condition = 'input.showPanel1',
                shiny::fileInput(inputId = 'tot_dat2',
                                 label = 'Upload Subject Level Data'),
                shiny::actionButton(inputId = "remove_adsl",
                                    label = "Remove ADSL",
                                    icon = icon("times"),
                                    style = paste0("color:#FFFFFF ; background-color: #377EB8;")
                )
              ),
              shiny::tags$br(),
              shiny::conditionalPanel(condition = "input.view == 'pie'",
                shiny::uiOutput("heightSlider"),
                shiny::uiOutput("numberRows")
              )
            ),
            shinyBS::bsCollapsePanel(
              shiny::HTML('<p style="color:white; font-size:100%;"> Modify data </p>'),
              shiny::tags$style(".fa-robot {color:#E43157} "),
              shiny::tags$style(".fa-refresh {color:#E43157} "),
              shiny::conditionalPanel(condition = "input.view == 'pie'",

                shinyWidgets::prettyToggle(inputId = 'AI.AdEPro',
                                           label_off = HTML("<span style='color: white; font-size: 15px;'> Add AdEPro AI </span>"),
                                           label_on = HTML("<span style = 'color: white;font-size: 15px;'> Hide AdEPro AI </span>"),
                                           value = FALSE,
                                           outline = TRUE,
                                           status_on = "default",
                                           status_off = "default",
                                           plain = TRUE,
                                           icon_off = icon("robot"),
                                           icon_on = icon ("robot")
                )
              ),
              shiny::conditionalPanel(condition = "output.flag > 0",
                shiny::fluidRow(
                  shiny::column(12,
                    shiny::conditionalPanel(condition = "input.view == 'pie'",
                      shinyWidgets::pickerInput(inputId = 'methSeq',
                                                label = 'Sequencing method',
                                                choices = sort(c('TSP',
                                                    'GW_single', 'GW_complete',
                                                    'GW_average', 'GW_ward',
                                                    'OLO_single', 'OLO_complete',
                                                    'OLO_average', 'OLO_ward',
                                                     'VAT')
                                                ),
                                                selected = 'OLO_single',
                                                multiple = FALSE,
                                                options = list(`live-search`=TRUE,
                                                               `style`='background: btn-primary',
                                                               `header`='Select item'
                                                )
                      )
                    )
                  )
                ),
                shiny::fluidRow(
                  shiny::column(12,
                    shiny::conditionalPanel(condition = "input.view == 'pie'",
                      shiny::uiOutput('varSeq')
                    )
                  )
                ),
                shiny::tags$br(),
                shiny::conditionalPanel(condition = "input.view == 'pie'",
                  shiny::actionButton(inputId = "AI.Update",
                                      label = "Update!",
                                      icon = icon("refresh"),
                                      style = paste0("color:#FFFFFF ; background-color: #377EB8;")
                  )
                )
              ),

              shiny::tags$br(),
              shiny::uiOutput("ae_type"),
              shiny::conditionalPanel(condition = "input.view == 'pie'",
                shiny::uiOutput("ae_sorting")
              )
            ),
            shinyBS::bsCollapsePanel(shiny::HTML('<p style="color:white; font-size:100%;"> Adverse Events for animation </p>'),
              shiny::uiOutput("ae_var"),
              shiny::conditionalPanel(condition = "input.view == 'pie'",
                shiny::uiOutput("ae_audio"),
                shiny::uiOutput("ae_sound1"),
                shiny::uiOutput("ae_sound2")
              )
            ),
            multiple = TRUE, id = "collapse", open = shiny::HTML('<p style="color:white; font-size:100%;"> Plot settings</p>'))
          ),
          shiny::column(1,
            shiny::uiOutput("speed")
          ),
          shiny::column(3,
            shiny::br(),
            shiny::uiOutput("slider"),
            shiny::conditionalPanel(condition = "output.load > 0",
              shinyWidgets::circleButton(inputId = "play",
                                         label = "",
                                         icon = icon("play"),
                                         no_outline = FALSE,
                                         status = "primary",
                                         size = "xs"
              ),
              shinyWidgets::circleButton(inputId = "pause",
                                         label = "",
                                         icon = icon("pause"),
                                         no_outline = FALSE,
                                         status = "primary",
                                         size = "xs"
              )
            )
          ),
          shiny::column(2,
            shiny::br(),
            shiny::tags$head(shiny::tags$style(shiny::HTML("#dayinfo {text-align: right;}"))
            ),
            uiOutput("dynamic_css"),
            shiny::verbatimTextOutput("dayinfo")
          ),
          shiny::column(1,
            shinyWidgets::radioGroupButtons(inputId = "view",
                                            label = shiny::HTML('<p style="color:white"> Change View: </p>'),
                                            choices = c(`<i class='fa fa-pie-chart'></i>` = "pie",
                                                        `<i class='fa fa-bar-chart'></i>` = "chart"),
                                            justified = TRUE,
                                            status = "primary"
            )
          ),
          shiny::column(1,
            shiny::plotOutput(outputId = "legend", width = "200px", height = "130px")
          ),
          shiny::column(2,
            adeproLogo(height = 135, width = 135, align = "right")
          )
        ),
        shiny::fluidRow(class = "myRow3",
          shiny::conditionalPanel(condition = "output.load == 0",
            shiny::tags$div(style="text-align:center",
              shiny::tags$div(shiny::HTML(paste(shiny::tags$span(style = "font-size:150%; color:white;","A 'shiny' Application for the (Audio-)Visualization of AdverseEvent Profiles")))
              ),
              adeproLogo(height = 360, width = 360, align = "right"),
              shiny::br(),
              shiny::tags$div(
               shiny::HTML(paste(shiny::tags$span(style = "font-size:150%; color:white;", "Please upload your data using the ",
                                           shiny::tags$span(style = "color:#E43157", "'Upload data'"),"-tab!", sep = "")))
              ),
             shiny::br(),
             shiny::HTML(paste0('<p style = "color: white"> <span style = "color:#E43157"> Note: </span> If upload as sas7bdat file fails due to <span style = "color:#E43157"> Maximum upload size (60MB) </span> exceeded, please convert to csv.</p>'))
            )
          ),
          shiny::conditionalPanel(condition = "output.load == 2",
            shiny::tags$div(style="text-align:center",
              shiny::br(),shiny::br(),
              shiny::br(),shiny::br(),
              shiny::br(),shiny::br(),
              shiny::HTML(paste(shiny::tags$span(style = "font-size:150%; color:white;", "",
                                                 shiny::tags$span(style = "color:#E43157", "Error: "),"Input data not as expected. Please check the AdEPro package manual!", sep = ""))
              )
            )
          ),
          shiny::conditionalPanel(condition = "output.load == 3",
            shiny::tags$div(style="text-align:center",
                            shiny::br(),shiny::br(),
                            shiny::br(),shiny::br(),
                            shiny::br(),shiny::br(),
                            shiny::HTML(paste(shiny::tags$span(style = "font-size:150%; color:white;", "",
                                                               shiny::tags$span(style = "color:#E43157", "Error: "),"Treatment variable has different properties in ADAE and ADSL data sets. Please check the AdEPro package manual!", sep = "")))
            )
          ),
          shiny::conditionalPanel(condition = "input.view == 'pie'",
            shiny::conditionalPanel(condition = "output.load > 0",
              shiny::uiOutput('patientpanel'),
              shiny::uiOutput('rowpanel')
            ),
            shiny::plotOutput(outputId = "slicePlots",
                              hover = hoverOpts(id ="plot_hover")
            )
          ),
          shiny::conditionalPanel(condition = "input.view == 'chart'",
            shiny::plotOutput("barchart")
          ),
          shiny::conditionalPanel(condition = "output.load > 0",
            shiny::uiOutput('zoompanel')
          )
        ), width = 12
      )
    )
  )

  #### Run Shiny App ####
  adepro_app <- shiny::shinyApp(ui = ui, server = server)
    if (!is.null(browser)) options(browser = browser)
    shiny::runApp(adepro_app, host = host, port = port)
}
