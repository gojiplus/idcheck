test_that("the published example gives the published check digit", {
  expect_identical(id_damm_digit("572"), "4")
  expect_identical(id_damm_valid(c("5724", "5725", "5274")), c(TRUE, FALSE, FALSE))
})

test_that("the table is a quasigroup with a zero diagonal", {
  expect_true(all(apply(damm_table, 1, sort) == 0:9))
  expect_true(all(apply(damm_table, 2, sort) == 0:9))
  expect_true(all(diag(damm_table) == 0))
})

test_that("every single-digit substitution of a valid number fails", {
  set.seed(2004)
  payloads <- c(sprintf("%02d", 0:99), random_digits(60, 1:24))
  valid <- paste0(payloads, id_damm_digit(payloads))
  expect_true(all(id_damm_valid(valid)))
  expect_false(any(id_damm_valid(single_substitutions(valid))))
})

test_that("every adjacent transposition of a valid number fails", {
  set.seed(2005)
  payloads <- c(sprintf("%02d", 0:99), random_digits(60, 1:24))
  valid <- paste0(payloads, id_damm_digit(payloads))
  wrong <- adjacent_transpositions(valid)
  expect_gt(length(wrong), 500)
  expect_false(any(id_damm_valid(wrong)))
})

test_that("C++ agrees with the R reference on 10,000 random inputs", {
  set.seed(31)
  payloads <- random_digits(4000, 1:30)
  x <- c(
    paste0(payloads, vapply(payloads, ref_damm_digit, character(1))),
    random_digits(4000, 1:30),
    random_string(2000, 0:20, c(0:9, 0:9, 0:9, " ", "-", "a", "X", "."))
  )
  expect_length(x, 10000)

  got <- id_damm_valid(x)
  expect_identical(got, unname(vapply(x, ref_damm_valid, logical(1))))
  expect_gt(sum(got), 4000)
  expect_identical(id_damm_digit(x), unname(vapply(x, ref_damm_digit, character(1))))
})
