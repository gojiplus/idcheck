test_that("MOD 11-2 reproduces the ORCID documentation's identifiers", {
  expect_identical(id_mod11_2_digit(c("000000021825009", "000000021694233")), c("7", "X"))
  expect_identical(
    id_mod11_2_valid(c("0000000218250097", "000000021694233X", "0000000218250098")),
    c(TRUE, TRUE, FALSE)
  )
})

test_that("MOD 11-2 accepts X only as an upper-case final character", {
  expect_identical(
    id_mod11_2_valid(c("000000021694233x", "00000002169423X3", "X")),
    c(FALSE, FALSE, FALSE)
  )
})

test_that("MOD 11-2 catches substitutions and adjacent transpositions", {
  set.seed(11)
  payloads <- random_digits(100, 1:24)
  valid <- paste0(payloads, id_mod11_2_digit(payloads))
  expect_true(all(id_mod11_2_valid(valid)))
  expect_false(any(id_mod11_2_valid(single_substitutions(valid[!grepl("X", valid)]))))
  expect_false(any(id_mod11_2_valid(adjacent_transpositions(valid))))
})

test_that("MOD 11-2 agrees with the R reference on 10,000 random inputs", {
  set.seed(32)
  payloads <- random_digits(4000, 1:30)
  x <- c(
    paste0(payloads, vapply(payloads, ref_mod11_2_digit, character(1))),
    random_string(4000, 1:30, c(0:9, 0:9, 0:9, "X")),
    random_string(2000, 0:20, c(0:9, 0:9, 0:9, " ", "-", "a", "X", "."))
  )
  expect_length(x, 10000)

  got <- id_mod11_2_valid(x)
  expect_identical(got, unname(vapply(x, ref_mod11_2_valid, logical(1))))
  expect_gt(sum(got), 4000)
  expect_identical(id_mod11_2_digit(x), unname(vapply(x, ref_mod11_2_digit, character(1))))
})

test_that("MOD 97-10 reproduces published examples", {
  expect_identical(id_mod97_10_digits("794"), "44")
  expect_identical(id_mod97_10_valid(c("79444", "79445")), c(TRUE, FALSE))

  # The fictitious IBAN GB82 WEST 1234 5698 7654 32, country and check digits
  # moved to the end as ISO 13616 prescribes.
  expect_identical(id_mod97_10_digits("WEST12345698765432GB"), "82")
  expect_true(id_mod97_10_valid("WEST12345698765432GB82"))
  expect_false(id_mod97_10_valid("WEST12345698765432GB28"))
})

test_that("MOD 97-10 wants upper-case letters and two final digits", {
  expect_identical(
    id_mod97_10_valid(c("west12345698765432gb82", "794A4", "44", "1")),
    c(FALSE, FALSE, FALSE, FALSE)
  )
  expect_identical(id_mod97_10_digits(c("", "ab", "A B")), rep(NA_character_, 3))
})

test_that("MOD 97-10 catches substitutions and adjacent transpositions", {
  set.seed(97)
  payloads <- random_digits(100, 1:24)
  valid <- paste0(payloads, id_mod97_10_digits(payloads))
  expect_true(all(id_mod97_10_valid(valid)))
  expect_false(any(id_mod97_10_valid(single_substitutions(valid))))
  expect_false(any(id_mod97_10_valid(adjacent_transpositions(valid))))
})

test_that("MOD 97-10 agrees with the R reference on 10,000 random inputs", {
  set.seed(33)
  payloads <- c(random_digits(2000, 1:30), random_string(2000, 1:30, c(0:9, LETTERS)))
  x <- c(
    paste0(payloads, vapply(payloads, ref_mod97_10_digits, character(1))),
    random_string(4000, 1:30, c(0:9, 0:9, LETTERS)),
    random_string(2000, 0:20, c(0:9, 0:9, 0:9, " ", "-", "a", "X", "."))
  )
  expect_length(x, 10000)

  got <- id_mod97_10_valid(x)
  expect_identical(got, unname(vapply(x, ref_mod97_10_valid, logical(1))))
  expect_gt(sum(got), 4000)
  expect_identical(id_mod97_10_digits(x), unname(vapply(x, ref_mod97_10_digits, character(1))))
})
