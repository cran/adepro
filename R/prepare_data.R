#' prepare_data - read SAS or CSV raw data and prepare the data sets
#' @description
#' Creates a list with two data sets 'pat_data' and 'ae_data' in format which is used in the AdEPro Application
#'
#' @param dat adae data set.
#' @param SUBJIDN Subject Id as numeric (required)
#' @param TRT01A Treatment Group as character (required)
#' @param SAFFN Safety Flag as numeric (required)
#' @param LVDT  Last Visit Date as numeric (required)
#' @param DTHDT Death Date as numeric (required)
#' @param TRTSDT Treatment Start Date as numeric (required)
#' @param AEDECOD Adverse Event Code as character (required)
#' @param AESTDY Adverse Event Start Date as numeric (required)
#' @param AETRTEMN Adverse Event Treatment Emergency Flag as numeric (required)
#' @param AEENDY Adverse Event End date as numeric (required)
#' @param AESEVN Adverse Event Severity Grade Flag as numeric (required)
#' @param AESERN Adverse Event Serious Flag as numeric (optional)
#' @param AERELN Adverse Event Related Flag as numeric (optional)
#' @param AERELPRN Adverse Event      Flag as numeric (optional)
#' @param AEACNN Adverse Event        Flag as numeric (optional)
#' @param optional_vars List optional Variables for the patient data set.
#' @param adsl_data Subject Level data set (optional)
#'
#' @keywords internal
#'

prepare_data <- function(dat,
                          SUBJIDN = "SUBJIDN",
                          TRT01A = "TRT01A",
                          SAFFN = "SAFFN",
                          LVDT = "LVDT",
                          DTHDT = "DTHDT",
                          TRTSDT = "TRTSDT",
                          AEDECOD = "AEDECOD",
                          AESTDY = "AESTDY",
                          AETRTEMN = "AETRTEMN",
                          AEENDY = "AEENDY",
                          AESEVN = "AESEVN",
                          AESERN = "AESERN",
                          AERELN = "AERELN",
                          AERELPRN = "AERELPRN",
                          AEACNN = "AEACNN",
                          optional_vars = NULL,
                          adsl_data = NULL) {
  cols <- treat <- ps <- patient <- day_start <- day_end <- output <- NULL

  if (!all(c("SUBJIDN", "TRT01A", "SAFFN", "AEDECOD", "AETRTEMN") %in% colnames(dat))) {
    return(list("ae_data" = as.data.frame("Input data not as expected. Please check the AdEPro package manual.",
                                          stringsAsFactors = FALSE),
                "pat_data" = as.data.frame("Input data not as expected. Please check the AdEPro package manual.",
                                           stringsAsFactors = FALSE)
                )
           )
  }

  if (is.numeric(dat$SUBJIDN) &
      (is.character(dat$TRT01A) | is.factor(dat$TRT01A)) &
      is.numeric(dat$SAFFN) &
      (is.character(dat$AEDECOD) | is.factor(dat$AEDECOD)) &
      is.numeric(dat$AETRTEMN)) {

  not_included <- 0
  which_not_included <- c()

  required_vars <- c(SUBJIDN, TRT01A, SAFFN, LVDT, DTHDT, TRTSDT,
                     AEDECOD, AESTDY, AETRTEMN, AEENDY, AESEVN)

  for (i in required_vars) {
    if (i %in% colnames(dat)) {
    } else {
      not_included <- not_included + 1
      which_not_included <- c(which_not_included, i)
    }
  }

  # check that no required variables are missing
  if (not_included == 0 & is.null(which_not_included)) {
    # required variables list for patient data
    var_list <- c("ps", "treat", "end", "death")
    pat_data <- dat %>%
      dplyr::filter(!!rlang::sym(SAFFN) == 1) %>%
      dplyr::mutate(ps = as.numeric(!!rlang::sym(SUBJIDN)),
                    treat = as.factor(!!rlang::sym(TRT01A)),
                    end = (as.numeric(!!rlang::sym(LVDT)) - as.numeric(!!rlang::sym(TRTSDT)) + 1),
                    death = ifelse(!is.na(!!rlang::sym(DTHDT)),
                                   as.numeric(!!rlang::sym(DTHDT)) - as.numeric(!!rlang::sym(TRTSDT)) + 1,
                                   99999)
      )

    pat_data <- pat_data[colSums(!is.na(pat_data)) > 0]

    if (is.null(optional_vars)) {
      optional_vars <- colnames(pat_data)
    }

    if (!is.null(adsl_data)) {
      pat_data2 <- adsl_data %>%
        dplyr::filter(!!rlang::sym(SAFFN) == 1) %>%
        dplyr::mutate(ps = as.numeric(!!rlang::sym(SUBJIDN)),
                      treat = as.factor(!!rlang::sym(TRT01A)),
                      end = (as.numeric(!!rlang::sym(LVDT)) - as.numeric(!!rlang::sym(TRTSDT)) + 1),
                      death = ifelse(!is.na(!!rlang::sym(DTHDT)),
                                     as.numeric(!!rlang::sym(DTHDT)) - as.numeric(!!rlang::sym(TRTSDT)) + 1,
                                     99999)
        )

      pat_data <- pat_data %>%
        dplyr::select(intersect(colnames(pat_data), colnames(pat_data2)))
      pat_data2 <- pat_data2 %>%
        dplyr::select(intersect(colnames(pat_data), colnames(pat_data2)))

      diff_subjid <- setdiff(pat_data2$SUBJIDN, pat_data$SUBJIDN)

      jointly_vars <- names(which(unlist(lapply(lapply(pat_data2,class), function(l){l[[1]]})) == unlist(lapply(lapply(pat_data, class),function(l){l[[1]]}))))


      if (all.equal(sort(unique(c(dat$TRT01A, ""))), sort(unique(c(adsl_data$TRT01A, ""))))[1] == TRUE) {
      pat_data <- rbind(pat_data %>%
              dplyr::select(jointly_vars),
            pat_data2 %>%
              dplyr::select(jointly_vars) %>%
              dplyr::filter(SUBJIDN %in% diff_subjid))
      } else {
        output <- "different TRT01A"
      }
    }

    if (!is.null(adsl_data)) {
      optional_vars <- colnames(pat_data)
    }

    for (i in unique(pat_data$ps)) {
      index <- apply(pat_data[pat_data$ps == i, optional_vars], 2, function(x){length(unique(x))})
      optional_vars <- intersect(optional_vars, names(index[index == 1]))
    }

    grouped_pat_dat <- pat_data %>%
      dplyr::group_by(treat) %>%
      tidyr::nest()
    tmp <- NULL

    for(i in 1:length(grouped_pat_dat)){
      tmp <- rbind(tmp, apply(grouped_pat_dat$data[[i]][, optional_vars[-which(optional_vars == "treat")]], 2, function(x){length(unique(x))}))
    }

    tmp2 <- apply(tmp,2,function(x){!all(x == 1)})
    optional_vars <- names(tmp2[tmp2 == TRUE])

    for (i in optional_vars) {
      if (i %in% colnames(dat)) {
        var_list <- c(var_list, i)
      }
    }

    #select only required variables for AdEPro
    pat_data <- pat_data %>%
      dplyr::select(var_list) %>%
      dplyr::arrange(treat, ps) %>%
      unique()

    ae_data <- dat %>%
      dplyr::filter(!!rlang::sym(SAFFN) == 1 & !!rlang::sym(AETRTEMN) == 1 & !is.na(!!rlang::sym(AEDECOD)) & !!rlang::sym(AEDECOD) != "") %>%
      dplyr::mutate(day_start = ifelse(!is.na(!!rlang::sym(AESTDY)), as.numeric(!!rlang::sym(AESTDY)), 1),
                    day_end = ifelse(!is.na(!!rlang::sym(AEENDY)), as.numeric(!!rlang::sym(AEENDY)),
                                     ifelse(!is.na(!!rlang::sym(AESTDY)) & as.numeric(!!rlang::sym(AESTDY)) > (as.numeric(!!rlang::sym(LVDT)) - as.numeric(!!rlang::sym(TRTSDT))),
                                            as.numeric(!!rlang::sym(AESTDY)), (as.numeric(!!rlang::sym(LVDT)) - as.numeric(!!rlang::sym(TRTSDT))))),
                    patient = as.numeric(!!rlang::sym(SUBJIDN)),
                    ae = as.factor(!!rlang::sym(AEDECOD)),
                    sev = ifelse(!is.na(!!rlang::sym(AESEVN)), as.numeric(!!rlang::sym(AESEVN)), 3),
                    trtem = ifelse(!!rlang::sym(AETRTEMN) == 1, 1, 0))

    #required variables for adverse event data
    var_list <- c("day_start", "day_end", "patient", "ae", "sev","trtem")

    #optional variables for adverse event data
    if (AESERN %in% colnames(dat)) {
      ae_data <- ae_data %>%
        dplyr::mutate(ser = ifelse(!!rlang::sym(AESERN) == 1, 1, 0),
                      nonser = ifelse(!!rlang::sym(AESERN) == 0, 1, 0))
      var_list <- c(var_list, c("ser", "nonser"))
    }

    if (AERELN %in% colnames(dat)) {
      ae_data <- ae_data %>%
        dplyr::mutate(studrel = ifelse(!!rlang::sym(AERELN) == 1, 1, 0))
      var_list <- c(var_list, c("studrel"))
    }

    if (AERELN %in% colnames(dat) & AESERN %in% colnames(dat)) {
      ae_data <- ae_data %>%
        dplyr::mutate(studrelser = ifelse(!!rlang::sym(AERELN) == 1, 1, 0) * ifelse(!!rlang::sym(AESERN) == 1, 1, 0))
      var_list <- c(var_list, c("studrelser"))
    }

    if (AERELPRN %in% colnames(dat)) {
      ae_data <- ae_data %>%
        dplyr::mutate(relprot = ifelse(!!rlang::sym(AERELPRN) == 1, 1, 0))
      var_list <- c(var_list, c("relprot"))
    }

    if (AEACNN %in% colnames(dat)) {
      ae_data <- ae_data %>%
        dplyr::mutate(resdisc = ifelse(!!rlang::sym(AEACNN) == 1, 1, 0))
      var_list <- c(var_list, c("resdisc"))
    }

    if (AERELN %in% colnames(dat) & AEACNN %in% colnames(dat)) {
      ae_data <- ae_data %>%
        dplyr::mutate(studrelresdisc = ifelse(!!rlang::sym(AERELN) == 1, 1 ,0) * ifelse(!!rlang::sym(AEACNN) == 1, 1, 0))
      var_list <- c(var_list, c("studrelresdisc"))
    }

    #select only the required variables for AdEPro
    ae_data <- ae_data %>%
      dplyr::select(var_list) %>%
      dplyr::arrange(patient, day_start, day_end)


    if(is.null(output)){
    return(list("ae_data" = as.data.frame(ae_data),
                "pat_data" = as.data.frame(pat_data)))
    } else {
      return(list("ae_data" = as.data.frame("Treatment variable has different properties in ADAE and ADSL data sets. Please check the AdEPro package manual.", stringsAsFactors = FALSE),
                  "pat_data" = as.data.frame("Treatment variable has different properties in ADAE and ADSL data sets. Please check the AdEPro package manual.", stringsAsFactors = FALSE)))
    }
  } else {
    ifelse(is.null(output),
    return(list("ae_data" = as.data.frame("Input data not as expected. Please check the AdEPro package manual.", stringsAsFactors = FALSE),
                "pat_data" = as.data.frame("Input data not as expected. Please check the AdEPro package manual.", stringsAsFactors = FALSE))),
    return(list("ae_data" = as.data.frame("Treatment variable has different properties in ADAE and ADSL data sets. Please check the AdEPro package manual.", stringsAsFactors = FALSE),
                "pat_data" = as.data.frame("Treatment variable has different properties in ADAE and ADSL data sets. Please check the AdEPro package manual.", stringsAsFactors = FALSE))))
  }
  } else {
    ifelse(is.null(output),
    return(list("ae_data" = as.data.frame("Input data not as expected. Please check the AdEPro package manual.", stringsAsFactors = FALSE),
                "pat_data" = as.data.frame("Input data not as expected. Please check the AdEPro package manual.", stringsAsFactors = FALSE))),
    return(list("ae_data" = as.data.frame("Treatment variable has different properties in ADAE and ADSL data sets. Please check the AdEPro package manual.", stringsAsFactors = FALSE),
                "pat_data" = as.data.frame("Treatment variable has different properties in ADAE and ADSL data sets. Please check the AdEPro package manual.", stringsAsFactors = FALSE))))
  }
}

