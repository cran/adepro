library(adepro)

context("basic")

test_that("launch_adepro is an appropriate function", {
  expect_that(launch_adepro, is.function)
  expect_warning(expect_error(launch_adepro(data)))
})

test_that("example_aedata is a valid input dataset for launch_adepro", {
  expect_that(example_aedata, is.data.frame)
  expect_equal(colnames(example_aedata), c("SUBJIDN", "AEDECOD", "AESTDY", "AEENDY", "AESEVN", "AETRTEMN", "AESERN", "AERELN", "TRT01A", "TRTSDT", "LVDT", "DTHDT", "AGE", "SEX", "REGION", "SAFFN"))
  expect_that(example_aedata$SUBJIDN, is.numeric)
  expect_that(example_aedata$AEDECOD, is.factor)
  expect_that(example_aedata$AESTDY, is.numeric)
  expect_that(example_aedata$AEENDY, is.numeric)
  expect_that(example_aedata$AESEVN, is.numeric)
  expect_that(example_aedata$AETRTEMN, is.numeric)
  expect_that(example_aedata$AESERN, is.numeric)
  expect_that(example_aedata$AERELN, is.numeric)
  expect_that(example_aedata$TRT01A, is.factor)
  expect_that(example_aedata$TRTSDT, is.numeric)
  expect_that(example_aedata$LVDT, is.numeric)
  expect_that(example_aedata$DTHDT, is.numeric)
  expect_that(example_aedata$AGE, is.numeric)
  expect_that(example_aedata$SEX, is.factor)
  expect_that(example_aedata$REGION , is.factor)
  expect_that(example_aedata$SAFFN , is.numeric)
  expect_true(all(example_aedata$AEENDY>= example_aedata$AESTDY))
})

test_that("example_sldata is a valid input dataset for launch_adepro", {
  expect_that(example_sldata, is.data.frame)
  expect_true(all(example_aedata$SUBJIDN %in% example_sldata$SUBJIDN))
})


test_that("adeprologo is an appropriate function", {
  expect_that(adeproLogo, is.function)
  Logo <- adeproLogo(height = 230, width = 160, align = "right")
  expect_output(str(Logo), "List of 3")
  expect_match(Logo[[1]], "div")
  expect_error(adeproLogo(height = numeric(0), width = 160, align = "right"), "argument height is of length zero")
  expect_error(adeproLogo(height = "230", width = 160, align = "right"), "height should be numeric")
  expect_error(adeproLogo(height = -230, width = 160, align = "right"), "height should be positive")
  expect_error(adeproLogo(height = 230, width = numeric(0), align = "right"), "argument width is of length zero")
  expect_error(adeproLogo(height = 230, width = "160", align = "right"), "width should be numeric")
  expect_error(adeproLogo(height = 230, width = -160, align = "right"), "width should be positive")
  expect_error(adeproLogo(height = 230, width = 160, align = 1), "Alignment need to be one of the following options: center, left, right")
  expect_error(adeproLogo(height = 230, width = 160, align = "centre"), "Alignment need to be one of the following options: center, left, right")
})
