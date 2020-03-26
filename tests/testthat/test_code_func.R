library(adepro)
context("functionality")

test_that("initQ", {
  Q <- initQ(ae_data = prepare_data(example_aedata)$ae_data)
  expect_true(is.data.frame(Q) | is.matrix(Q))
  expect_true(all(Q %in% c("TRUE", "FALSE")))
  expect_equal(nrow(example_aedata), nrow(Q))
})

test_that("check_data", {
  df <- prepare_data(dat=example_aedata, adsl_data = example_sldata)
  expect_silent(check_data(df$ae_data, df$pat_data))
})

test_that("set_vector_layout", {
   vec_lay <- set_vector_layout(prepare_data(dat=example_aedata, adsl_data = example_sldata)$pat_data, height = 12)
   expect_true(is.vector(vec_lay) & is.numeric(vec_lay))
   expect_equal(nrow(example_sldata), max(vec_lay))
   expect_equal(vec_lay[which(vec_lay != 0)], 1:nrow(example_sldata))
})

test_that("set_group_lines", {
  grp_l <- set_group_lines(prepare_data(dat=example_aedata, adsl_data = example_sldata)$pat_data, height = 12)
  n_trt <- length(unique(prepare_data(dat=example_aedata, adsl_data = example_sldata)$pat_data$treat))
  expect_equal(length(grp_l), 3)
  expect_equal(n_trt-1, ncol(grp_l[[1]]))
  expect_equal(n_trt-1, ncol(grp_l[[2]]))
  expect_equal(2*(n_trt-1), length(grp_l[[3]]))
})

test_that("set_width", {
  width <- set_width(prepare_data(dat=example_aedata, adsl_data = example_sldata)$pat_data, height = 12)
  expect_true(is.numeric(width))
  expect_true(all(width == 1))
  expect_true(length(width) > 0)
})

test_that("set_global_params", {
  df <- prepare_data(dat=example_aedata, adsl_data = example_sldata)
  global <- set_global_params(df$ae_data, df$pat_data)
  Q <- initQ(ae_data = df$ae_data)
  height <- global$height
  width <- set_width(df$pat_data, height=height)
  grp_l <- set_group_lines(df$pat_data, height=height)
  glb_names <- c("titles", "footnote", "Q", "AE_options", "width", "height", "xlines", "ylines", "plines")
  expect_equal(names(global), glb_names)
  expect_equal(Q, global$Q)
  expect_equal(width, global$width)
  expect_equal(grp_l[[1]], global$xlines[[1]])
  expect_equal(grp_l[[2]], global$ylines[[1]])
  expect_equal(grp_l[[3]], global$plines[[1]])
})

test_that("preproc_ae", {
  prep_ae <- prepare_data(dat=example_aedata, adsl_data = example_sldata)$ae_data
  example_aedata_n <- preproc_ae(prep_ae)
  expect_equal(prep_ae[,1:5], example_aedata_n[,1:5])
  expect_true(all(example_aedata_n$r >= 0 & example_aedata_n$r <= 1))
  expect_true(all(is.na(example_aedata_n$d)))
})

test_that("preproc_patients", {
  patient_n <- preproc_patients(prepare_data(dat=example_aedata, adsl_data = example_sldata)$pat_data, height = 12)
  expect_equal(prepare_data(dat=example_aedata, adsl_data = example_sldata)$pat_data[,1:4], patient_n[,1:4])
  expect_equal(names(patient_n)[11:12], c("X", "Y"))
  expect_equal(patient_n$X, sort(patient_n$X))
  expect_true(all(patient_n$Y < 0))
  expect_true(min(abs(diff(patient_n$Y)))==2)
})

test_that("ae_count", {
  df <- prepare_data(dat=example_aedata, adsl_data = example_sldata)
  count <- ae_count(df$ae_data, df$pat_data)
  n_trt <- length(unique(count$treat))
  expect_true(is.data.frame(count))
  expect_true(ncol(count)==3)
  expect_equal(colnames(count), c("day", "treat", "freq"))
  expect_equal(nrow(count), n_trt*max(count$day))
  expect_true(is.numeric(count$freq))
  expect_true(all(count$freq >= 0 & count$freq <=  4))
})

test_that("bar_chart", {
  plot(0, 0)
  df <- prepare_data(dat=example_aedata, adsl_data = example_sldata)
  vars <- unique(df$ae_data$ae)[1:8]
  expect_silent(bar_chart(ae_data =  df$ae_data,
                          patients = df$pat_data,
                          day = 5,
                          variables = vars,
                          day_max = 495,
                          count_max = 32,
                          cex.n = 2))
})

test_that("count_event", {
  df <- prepare_data(dat=example_aedata, adsl_data = example_sldata)
  vars <- unique(df$ae_data$ae)[1:8]
  tot <- df$ae_data %>%
    dplyr::right_join((df$pat_data %>%
                         dplyr::rename(patient = ps)) %>%
                        dplyr::select(patient, treat), by = 'patient') %>%
    dplyr::filter(ae %in% vars)

  tot$ae <- factor(tot$ae, levels = vars)
  cnt_evnt <- count_event(total = tot, day = 5)
  expect_true(is.data.frame(cnt_evnt))
  expect_true("n" %in% colnames(cnt_evnt))
  expect_true(is.factor(cnt_evnt$treat))
  expect_true(is.factor(cnt_evnt$ae))
})

test_that("order_patient", {
  df <- prepare_data(dat=example_aedata, adsl_data = example_sldata)
  vars <- unique(df$ae_data$ae)[1:8]
  ord_pat <- order_patient(ae_data = df$ae_data,
                patients = df$pat_data,
                variables = vars,
                method_dist = 'euclidean',
                method_seriate = 'TSP')
  expect_that(ord_pat, is.data.frame)
  expect_true("SEQUENCING" %in% colnames(ord_pat))
  expect_true("patient" %in% colnames(ord_pat))
  expect_equal(nrow(ord_pat), nrow(example_sldata))
})

test_that("prepare_data", {
    expect_that(prepare_data, is.function)
    total <- prepare_data(example_aedata)
    expect_output(str(total), "List of 2")
})

test_that("pie_legend", {
  plot(0, 0)
  df <- prepare_data(dat=example_aedata, adsl_data = example_sldata)
  vars <- unique(df$ae_data$ae)[1:8]
  expect_silent(pie_legend(aes=vars))
})



