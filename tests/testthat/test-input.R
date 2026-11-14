valid_fns <- list(
  id_luhn_valid, id_verhoeff_valid, id_damm_valid, id_mod11_2_valid, id_mod97_10_valid
)
digit_fns <- list(
  id_luhn_digit, id_verhoeff_digit, id_damm_digit, id_mod11_2_digit, id_mod97_10_digits
)

test_that("NA in gives NA out", {
  x <- c(NA, "2363", NA_character_)
  for (f in valid_fns) {
    expect_identical(is.na(f(x)), c(TRUE, FALSE, TRUE))
  }
  for (f in digit_fns) {
    expect_identical(is.na(f(x)), c(TRUE, FALSE, TRUE))
  }
  for (scheme in id_schemes()$scheme) {
    expect_identical(id_valid(x, scheme), c(NA, FALSE, NA))
  }
})

test_that("empty input gives empty output of the right type", {
  for (f in valid_fns) {
    expect_identical(f(character()), logical())
  }
  for (f in digit_fns) {
    expect_identical(f(character()), character())
  }
  expect_identical(id_valid(character(), "in_pan"), logical())
})

test_that("hostile values are FALSE or NA, never an error", {
  x <- hostile_values()
  for (f in valid_fns) {
    expect_no_condition(out <- f(x))
    expect_type(out, "logical")
    expect_false(anyNA(out))
  }
  for (f in digit_fns) {
    expect_no_condition(out <- f(x))
    expect_type(out, "character")
    expect_length(out, length(x))
  }
  for (scheme in id_schemes()$scheme) {
    expect_no_condition(out <- id_valid(x, scheme))
    expect_identical(out, rep(FALSE, length(x)), info = scheme)
  }
})

test_that("the bare algorithms do not strip separators or accept other scripts", {
  expect_false(id_verhoeff_valid("23 63"))
  expect_false(id_luhn_valid("7992-7398-713"))
  expect_true(all(is.na(id_luhn_digit(c("", "12a", " 12", "१२")))))
  expect_false(id_valid("९९९९४१०५७०५८", "in_aadhaar"))
})

test_that("a single digit is a payload but not a checked number", {
  for (f in valid_fns) {
    expect_false(f("0"))
  }
  expect_identical(id_luhn_digit("0"), "0")
  expect_identical(id_verhoeff_digit("0"), "4")
  expect_identical(id_damm_digit("0"), "0")
})

test_that("very long numbers are handled without overflow", {
  set.seed(5)
  payload <- random_digits(1, 5000)
  for (i in seq_along(valid_fns)) {
    whole <- paste0(payload, digit_fns[[i]](payload))
    expect_true(valid_fns[[i]](whole))
  }
  expect_identical(id_luhn_digit(payload), ref_luhn_digit(payload))
  expect_identical(id_mod97_10_digits(payload), ref_mod97_10_digits(payload))
})

test_that("names and other attributes are not carried over", {
  x <- c(a = "2363", b = "x")
  expect_null(attributes(id_verhoeff_valid(x)))
  expect_null(attributes(id_valid(x, "in_pan")))
})

test_that("input that is not character is an error", {
  expect_error(
    id_valid(999941057058, "in_aadhaar"),
    "must be a character vector",
    class = "rlang_error"
  )
  expect_error(id_valid(factor("AAAPL1234C"), "in_pan"), "must be a character vector")
  expect_error(id_valid(NULL, "in_pan"), "must be a character vector")
  expect_error(id_luhn_valid(79927398713), "must be a character vector")
  expect_error(id_verhoeff_digit(236L), "must be a character vector")
})

test_that("an unknown or malformed scheme is an error", {
  expect_error(id_valid("x", "in_ssn"), "must be one of")
  expect_error(id_valid("x", c("in_pan", "in_epic")))
  expect_error(id_valid("x"))
})
