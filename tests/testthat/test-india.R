# No number here belongs to a person. Aadhaar examples are the test UIDs UIDAI
# publishes for its authentication sandbox; everything else is a placeholder
# from official examples or is built by computing a check digit.
uidai_test_uids <- c(
  "999941057058", "999971658847", "999933119405", "999955183433",
  "999990501894", "999999990019"
)

test_that("Aadhaar: UIDAI's test numbers pass", {
  expect_true(all(id_valid(uidai_test_uids, "in_aadhaar")))
})

test_that("Aadhaar: spaces and hyphens between groups are ignored", {
  expect_identical(
    id_valid(c("9999 4105 7058", "9999-4105-7058", " 999941057058 "), "in_aadhaar"),
    c(TRUE, TRUE, TRUE)
  )
})

test_that("Aadhaar: a changed or swapped digit fails", {
  expect_false(any(id_valid(single_substitutions(uidai_test_uids), "in_aadhaar")))
  expect_false(any(id_valid(adjacent_transpositions(uidai_test_uids), "in_aadhaar")))
})

test_that("Aadhaar: a first digit of 0 or 1 fails even with a correct check digit", {
  payloads <- c("09994105705", "19994105705")
  x <- paste0(payloads, id_verhoeff_digit(payloads))
  expect_identical(id_verhoeff_valid(x), c(TRUE, TRUE))
  expect_identical(id_valid(x, "in_aadhaar"), c(FALSE, FALSE))
})

test_that("Aadhaar: wrong lengths fail", {
  payloads <- c("9999410570", "999941057058")
  x <- paste0(payloads, id_verhoeff_digit(payloads))
  expect_identical(nchar(x), c(11L, 13L))
  expect_identical(id_valid(x, "in_aadhaar"), c(FALSE, FALSE))
})

test_that("PAN: the fourth character must be a holder type", {
  expect_true(id_valid("AAAPL1234C", "in_pan"))
  expect_false(id_valid("ABCDE1234F", "in_pan"))

  x <- paste0("AAA", LETTERS, "L1234C")
  holder_types <- c("A", "B", "C", "F", "G", "H", "J", "L", "P", "T")
  expect_identical(id_valid(x, "in_pan"), LETTERS %in% holder_types)
})

test_that("PAN: the shape is five letters, four digits, one letter", {
  expect_identical(
    id_valid(
      c(
        "aaapl1234c", "AAAPL 1234 C", "AAAPL12345", "AAAP11234C", "AAAPL1234", "AAAPL1234CC",
        "1AAPL1234C"
      ),
      "in_pan"
    ),
    c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
  )
})

test_that("GSTIN: a constructed number passes and other check characters fail", {
  valid <- make_gstin("27", "AAAPL1234C")
  expect_true(id_valid(valid, "in_gstin"))
  expect_true(id_valid(tolower(valid), "in_gstin"))

  others <- setdiff(c(0:9, LETTERS), substr(valid, 15, 15))
  expect_false(any(id_valid(paste0(substr(valid, 1, 14), others), "in_gstin")))
})

test_that("GSTIN: the placeholder printed in guides has the wrong check character", {
  expect_false(id_valid("22AAAAA0000A1Z5", "in_gstin"))
  expect_true(id_valid("22AAAAA0000A1ZC", "in_gstin"))
})

test_that("GSTIN: state codes are 01 to 38 and 97", {
  states <- sprintf("%02d", 0:99)
  x <- make_gstin(states, "AAAPL1234C")
  expect_identical(id_valid(x, "in_gstin"), 0:99 %in% c(1:38, 97))
})

test_that("GSTIN: the embedded PAN, entity number and fourteenth character are checked", {
  expect_false(id_valid(make_gstin("27", "ABCDE1234F"), "in_gstin"))
  expect_false(id_valid(make_gstin("27", "AAAPL1234C", entity = "0"), "in_gstin"))
  expect_true(all(id_valid(make_gstin("27", "AAAPL1234C", entity = c(1:9, LETTERS)), "in_gstin")))
  expect_identical(
    id_valid(make_gstin("27", "AAAPL1234C", kind = LETTERS), "in_gstin"),
    LETTERS %in% c("Z", "C", "D")
  )
})

test_that("EPIC: three letters then seven digits", {
  expect_identical(
    id_valid(
      c("ABC1234567", "abc1234567", "AB12345678", "ABCD123456", "ABC123456", "ABC12345678"),
      "in_epic"
    ),
    c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
  )
})

test_that("passport: one letter then seven digits", {
  expect_identical(
    id_valid(
      c("A1234567", "a1234567", "12345678", "AB123456", "A123456", "A12345678"),
      "in_passport"
    ),
    c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
  )
})

test_that("mobile: ten digits starting 6 to 9, with an optional prefix", {
  expect_identical(
    id_valid(
      c(
        "9876543210", "6876543210", "+91 98765 43210", "+91-98765-43210", "919876543210",
        "09876543210"
      ),
      "in_mobile"
    ),
    rep(TRUE, 6)
  )
  expect_identical(
    id_valid(
      c(
        "5876543210", "0876543210", "987654321", "98765432100", "+9198765432100",
        "+919876543", "00919876543210", "+929876543210", "98765A3210"
      ),
      "in_mobile"
    ),
    rep(FALSE, 9)
  )
})

test_that("every scheme agrees with the R reference on 10,000 random inputs", {
  set.seed(91)
  aadhaar_payload <- paste0(sample(2:9, 2000, replace = TRUE), random_digits(2000, 10))
  aadhaar <- paste0(aadhaar_payload, vapply(aadhaar_payload, ref_verhoeff_digit, character(1)))
  spaced <- sub("^(.{4})(.{4})(.{4})$", "\\1 \\2 \\3", aadhaar[1:500])

  pan <- paste0(
    random_string(1500, 3, LETTERS), sample(LETTERS, 1500, replace = TRUE),
    sample(LETTERS, 1500, replace = TRUE), random_digits(1500, 4),
    sample(LETTERS, 1500, replace = TRUE)
  )
  gstin <- make_gstin(
    sprintf("%02d", sample(0:40, 1500, replace = TRUE)), pan,
    entity = sample(c(0:9, LETTERS), 1500, replace = TRUE),
    kind = sample(c("Z", "Z", "Z", "C", "D", "A"), 1500, replace = TRUE)
  )
  corrupted <- gstin[1:500]
  substr(corrupted, 15, 15) <- sample(c(0:9, LETTERS), 500, replace = TRUE)

  x <- c(
    aadhaar, spaced, random_digits(1500, 12), pan, gstin, corrupted,
    random_string(500, 10, c(LETTERS, 0:9)),
    paste0(random_string(500, 3, LETTERS), random_digits(500, 7)),
    paste0(sample(LETTERS, 500, replace = TRUE), random_digits(500, 7)),
    paste0(
      sample(c("", "+91", "91", "0", "+"), 500, replace = TRUE),
      sample(4:9, 500, replace = TRUE), random_digits(500, c(9, 9, 9, 8, 10))
    ),
    random_string(496, 0:20, c(0:9, 0:9, LETTERS, letters, " ", "-", "+", ".")),
    hostile_values()[1:4]
  )
  expect_length(x, 10000)

  for (scheme in id_schemes()$scheme) {
    got <- id_valid(x, scheme)
    expect_identical(got, ref_scheme_valid(x, scheme), info = scheme)
    expect_gt(sum(got), 100)
  }
})
