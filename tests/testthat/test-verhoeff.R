test_that("published examples give the published check digits", {
  expect_identical(id_verhoeff_digit(c("236", "12345", "142857")), c("3", "1", "0"))
  expect_identical(id_verhoeff_valid(c("2363", "123451", "1428570")), c(TRUE, TRUE, TRUE))
  expect_identical(id_verhoeff_valid(c("2364", "123450", "1428571")), c(FALSE, FALSE, FALSE))
})

test_that("the generated tables are a group and a permutation", {
  t <- verhoeff_tables
  expect_true(all(apply(t$d, 1, sort) == 0:9))
  expect_true(all(apply(t$d, 2, sort) == 0:9))
  expect_true(all(apply(t$p, 1, sort) == 0:9))
  expect_identical(as.integer(t$p[2, ]), c(1L, 5L, 7L, 6L, 2L, 8L, 3L, 0L, 9L, 4L))
})

test_that("every single-digit substitution of a valid number fails", {
  set.seed(1969)
  payloads <- c(sprintf("%02d", 0:99), random_digits(60, 1:24))
  valid <- paste0(payloads, id_verhoeff_digit(payloads))
  expect_true(all(id_verhoeff_valid(valid)))

  wrong <- single_substitutions(valid)
  expect_gt(length(wrong), 5000)
  expect_false(any(id_verhoeff_valid(wrong)))
})

test_that("every adjacent transposition of a valid number fails", {
  set.seed(1970)
  payloads <- c(sprintf("%02d", 0:99), random_digits(60, 1:24))
  valid <- paste0(payloads, id_verhoeff_digit(payloads))

  wrong <- adjacent_transpositions(valid)
  expect_gt(length(wrong), 500)
  expect_false(any(id_verhoeff_valid(wrong)))
})

test_that("C++ agrees with the R reference on 10,000 random inputs", {
  set.seed(29)
  payloads <- random_digits(4000, 1:30)
  x <- c(
    paste0(payloads, vapply(payloads, ref_verhoeff_digit, character(1))),
    random_digits(4000, 1:30),
    random_string(2000, 0:20, c(0:9, 0:9, 0:9, " ", "-", "a", "X", "."))
  )
  expect_length(x, 10000)

  got <- id_verhoeff_valid(x)
  expect_identical(got, unname(vapply(x, ref_verhoeff_valid, logical(1))))
  expect_gt(sum(got), 4000)
  expect_gt(sum(!got), 4000)

  expect_identical(id_verhoeff_digit(x), unname(vapply(x, ref_verhoeff_digit, character(1))))
})

test_that("CheckDigit agrees on random digit strings", {
  skip_if_not_installed("CheckDigit")
  set.seed(7)
  payloads <- random_digits(1000, 1:30)
  x <- c(paste0(payloads, id_verhoeff_digit(payloads)), random_digits(1000, 2:30))
  expect_identical(
    id_verhoeff_valid(x),
    unname(CheckDigit::VerifyCheckDigit(x, "Verhoeff"))
  )
})
