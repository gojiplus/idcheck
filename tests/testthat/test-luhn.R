test_that("published examples give the published check digits", {
  expect_identical(id_luhn_digit("7992739871"), "3")
  expect_identical(
    id_luhn_valid(c("79927398713", "4111111111111111", "79927398710", "4111111111111112")),
    c(TRUE, TRUE, FALSE, FALSE)
  )
})

test_that("every single-digit substitution of a valid number fails", {
  set.seed(1960)
  payloads <- c(sprintf("%02d", 0:99), random_digits(60, 1:24))
  valid <- paste0(payloads, id_luhn_digit(payloads))
  expect_true(all(id_luhn_valid(valid)))
  expect_false(any(id_luhn_valid(single_substitutions(valid))))
})

test_that("adjacent transpositions fail, except 09 with 90", {
  set.seed(1961)
  payloads <- c(sprintf("%02d", 0:99), random_digits(60, 1:24))
  valid <- paste0(payloads, id_luhn_digit(payloads))

  swapped <- unlist(lapply(valid, function(s) {
    out <- adjacent_transpositions(s)
    ch <- strsplit(s, "", fixed = TRUE)[[1]]
    pairs <- paste0(ch[-length(ch)], ch[-1])
    pairs <- pairs[ch[-1] != ch[-length(ch)]]
    stats::setNames(out, pairs)
  }))
  blind <- names(swapped) %in% c("09", "90")

  expect_gt(sum(blind), 0)
  expect_true(all(id_luhn_valid(swapped[blind])))
  expect_false(any(id_luhn_valid(swapped[!blind])))
})

test_that("C++ agrees with the R reference on 10,000 random inputs", {
  set.seed(30)
  payloads <- random_digits(4000, 1:30)
  x <- c(
    paste0(payloads, vapply(payloads, ref_luhn_digit, character(1))),
    random_digits(4000, 1:30),
    random_string(2000, 0:20, c(0:9, 0:9, 0:9, " ", "-", "a", "X", "."))
  )
  expect_length(x, 10000)

  got <- id_luhn_valid(x)
  expect_identical(got, unname(vapply(x, ref_luhn_valid, logical(1))))
  expect_gt(sum(got), 4000)
  expect_identical(id_luhn_digit(x), unname(vapply(x, ref_luhn_digit, character(1))))
})

test_that("checkLuhn agrees on random digit strings", {
  skip_if_not_installed("checkLuhn")
  set.seed(8)
  payloads <- random_digits(150, 2:25)
  # checkLuhn() rejects anything of two characters or fewer, so start at three.
  x <- c(paste0(payloads, id_luhn_digit(payloads)), random_digits(150, 3:25))
  expect_identical(
    id_luhn_valid(x),
    vapply(x, checkLuhn::checkLuhn, logical(1), USE.NAMES = FALSE)
  )
})
