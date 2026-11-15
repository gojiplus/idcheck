test_that("id_schemes() describes each scheme once", {
  s <- id_schemes()
  expect_s3_class(s, "data.frame")
  expect_named(s, c("scheme", "country", "name", "check", "algorithm", "source"))
  expect_false(anyDuplicated(s$scheme) > 0)
  expect_match(s$scheme, "^([a-z]{2}|intl)_[a-z0-9_]+$")
  national <- !startsWith(s$scheme, "intl_")
  expect_identical(toupper(substr(s$scheme[national], 1, 2)), s$country[national])
  expect_true(all(is.na(s$country[!national])))
  expect_in(s$check, c("checksum", "format"))
  expect_identical(is.na(s$algorithm), s$check == "format")
  expect_false(anyNA(s$source))
})

test_that("each scheme name reaches its own rule", {
  examples <- c(
    in_aadhaar = "999941057058",
    in_pan = "AAAPL1234C",
    in_gstin = "22AAAAA0000A1ZC",
    in_epic = "ABC1234567",
    in_passport = "A1234567",
    in_mobile = "6123456789",
    us_ssn = "536-90-4399",
    gb_nhs = "401 023 2137",
    intl_payment_card = "4111 1111 1111 1111"
  )
  expect_setequal(names(examples), id_schemes()$scheme)

  hits <- vapply(
    names(examples),
    function(scheme) id_valid(unname(examples), scheme),
    logical(length(examples))
  )
  expect_identical(unname(diag(hits)), rep(TRUE, length(examples)))
  expect_identical(sum(hits), length(examples))
})
