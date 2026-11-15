test_that("Social Security numbers exclude the ranges SSA never issues", {
  expect_true(all(id_valid(c("536-90-4399", "001-01-0001", "123-45-6788"), "us_ssn")))
  never <- c(
    "000-12-3456", "666-12-3456", "900-12-3456", "999-12-3456", "123-00-4567",
    "123-45-0000", "12345678", "1234567890"
  )
  placeholders <- c("123-45-6789", "987-65-4320", "078-05-1120", "111-11-1111")
  expect_false(any(id_valid(never, "us_ssn")))
  expect_false(any(id_valid(placeholders, "us_ssn")))
})

test_that("NHS numbers carry a modulus 11 check digit", {
  # The example from the NHS Data Dictionary, and the same with each last digit.
  expect_true(id_valid("943 476 5919", "gb_nhs"))
  others <- paste0("943476591", setdiff(0:9, 9))
  expect_false(any(id_valid(others, "gb_nhs")))
})

test_that("IBANs are checked by country length and MOD 97-10", {
  # Examples published in the SWIFT IBAN registry.
  valid <- c(
    "GB82 WEST 1234 5698 7654 32", "DE89 3704 0044 0532 0130 00",
    "FR14 2004 1010 0505 0001 3M02 606", "NL91 ABNA 0417 1643 00",
    "MT84 MALT 0110 0001 2345 MTLC AST0 01S"
  )
  expect_true(all(id_valid(valid, "intl_iban")))
  wrong_check <- "GB83 WEST 1234 5698 7654 32"
  wrong_length <- "GB82 WEST 1234 5698 7654 3"
  unknown <- "QQ82 WEST 1234 5698 7654 32"
  expect_false(any(id_valid(c(wrong_check, wrong_length, unknown), "intl_iban")))
})

test_that("card numbers need an issuer prefix, a length and a Luhn digit", {
  # Test numbers the networks publish.
  valid <- c(
    "4111 1111 1111 1111", "5555 5555 5555 4444", "2223 0031 2200 3222",
    "3782 822463 10005", "6011 1111 1111 1117", "3530 1113 3330 0000",
    "6200 0000 0000 0005"
  )
  expect_true(all(id_valid(valid, "intl_payment_card")))
  luhn_only <- "1234 5678 9012 3452"
  expect_true(id_luhn_valid("1234567890123452"))
  expect_false(id_valid(luhn_only, "intl_payment_card"))
  expect_false(id_valid("4111 1111 1111 1112", "intl_payment_card"))
  expect_false(id_valid("3782 822463 1000", "intl_payment_card"))
})

test_that("random digits rarely pass as cards", {
  set.seed(1)
  digits <- vapply(1:20000, function(i) paste(sample(0:9, 16, TRUE), collapse = ""), "")
  expect_lt(mean(id_valid(digits, "intl_payment_card")), 0.08)
})

test_that("the international schemes agree with the R reference", {
  set.seed(42)
  ssn <- paste0(random_digits(1500, 9))
  nhs <- random_digits(1500, 10)
  cards <- c(
    paste0(
      sample(c("4", "51", "37", "6011", "35", "62", "60", "36"), 2000, TRUE),
      random_digits(2000, 11:17)
    ),
    "4111111111111111", "5555555555554444"
  )
  # Valid IBANs built with the reference arithmetic, for GB, DE, FR and NL.
  countries <- c(GB = 22L, DE = 22L, FR = 27L, NL = 18L)
  built <- vapply(seq_len(400), function(i) {
    country <- names(countries)[i %% 4 + 1]
    account <- random_string(1, countries[[country]] - 4, c(LETTERS, 0:9))
    check <- 98 - ref_mod97_remainder(paste0(account, country, "00"))
    paste0(country, sprintf("%02d", check), account)
  }, character(1))
  ibans <- c(
    built, "GB82WEST12345698765432", "DE89370400440532013000",
    paste0(
      sample(c("GB", "DE", "FR", "NL", "QQ"), 1500, TRUE), random_digits(1500, 2),
      random_string(1500, c(14, 18, 20, 23), c(LETTERS, 0:9))
    )
  )
  noise <- random_string(500, 0:20, c(0:9, LETTERS, " ", "-"))
  x <- c(ssn, nhs, cards, ibans, noise, hostile_values()[1:4])
  for (scheme in c("us_ssn", "gb_nhs", "intl_iban", "intl_payment_card")) {
    got <- id_valid(x, scheme)
    expect_identical(got, ref_scheme_valid(x, scheme), info = scheme)
    expect_gt(sum(got, na.rm = TRUE), 20)
  }
})
