#' Test whether strings are valid identity numbers
#'
#' @description
#' `id_valid()` answers "is this string a well-formed number under this
#' scheme?" for each element of a character vector. Where the scheme has a
#' published check digit, it is verified; otherwise only the format is. See
#' [id_schemes()] for which is which.
#'
#' A `TRUE` means the string has the right form. It does not mean the number
#' was ever issued, or to whom.
#'
#' @details
#' Before a value is tested, ASCII whitespace and hyphens are removed and
#' letters are upper-cased, so `"9999 4105 7058"` is judged as `999941057058`.
#'
#' The rules, with the source for each, are in the package's `SPEC.md`:
#'
#' * `in_aadhaar`: 12 digits, the first 2 to 9, the last a Verhoeff check
#'   digit.
#' * `in_pan`: `AAAAA9999A`, the fourth character one of `A`, `B`, `C`, `F`,
#'   `G`, `H`, `J`, `L`, `P`, `T` (the kind of holder). The final check letter
#'   is not verified because its algorithm is not published.
#' * `in_gstin`: 15 characters: state code `01` to `38` or `97`, a PAN, an
#'   entity number, one of `Z`, `C`, `D`, and a Luhn mod 36 check character.
#' * `in_epic`: three letters and seven digits.
#' * `in_passport`: one letter and seven digits.
#' * `in_mobile`: ten digits, the first 6 to 9, optionally after `+91`, `91`
#'   or `0`.
#'
#' @param x A character vector.
#' @param scheme A single string naming the scheme, one of
#'   `id_schemes()$scheme`.
#'
#' @returns A logical vector the same length as `x`: `TRUE` for a well-formed
#'   number, `FALSE` for anything else, and `NA` where `x` is `NA`. Malformed
#'   values never raise an error.
#'
#' @seealso [id_schemes()] for the schemes, [id_verhoeff_valid()] and its
#'   siblings for the bare check-digit algorithms.
#' @export
#' @examples
#' # A test number published by UIDAI, and the same with one digit changed
#' id_valid(c("9999 4105 7058", "9999 4105 7059", NA), "in_aadhaar")
#'
#' # The fourth character of a PAN must be a holder type; D is not one
#' id_valid(c("AAAPL1234C", "ABCDE1234F"), "in_pan")
#'
#' id_valid(c("+91 98765 43210", "12345 67890"), "in_mobile")
#'
#' # Share of a column that passes each scheme
#' x <- c("999941057058", "999971658847", "123456789012", "not a number")
#' vapply(id_schemes()$scheme, function(s) mean(id_valid(x, s)), numeric(1))
id_valid <- function(x, scheme) {
  check_character(x)
  scheme <- rlang::arg_match0(scheme, scheme_names)
  scheme_valid_(x, match(scheme, scheme_names) - 1L)
}

#' List the identity-number schemes
#'
#' @returns A data frame with one row per scheme and the columns
#'   * `scheme`: the name to pass to [id_valid()], `<country>_<scheme>`.
#'   * `country`: ISO 3166-1 alpha-2 code.
#'   * `name`: what the number is called.
#'   * `check`: `"checksum"` if a check digit is verified, `"format"` if only
#'     the shape is.
#'   * `algorithm`: the check-digit algorithm, or `NA`.
#'   * `source`: where the rule comes from.
#' @export
#' @examples
#' id_schemes()[, c("scheme", "name", "check")]
id_schemes <- function() {
  data.frame(
    scheme = scheme_names,
    country = "IN",
    name = c(
      "Aadhaar number",
      "Permanent Account Number (PAN)",
      "Goods and Services Tax Identification Number (GSTIN)",
      "Elector's Photo Identity Card (EPIC) number",
      "Passport number",
      "Mobile phone number"
    ),
    check = c("checksum", "format", "checksum", "format", "format", "format"),
    algorithm = c("Verhoeff", NA, "Luhn mod 36", NA, NA, NA),
    source = c(
      "UIDAI, A UID Numbering Scheme (2010)",
      "Income Tax Department, Permanent Account Number tutorial",
      "GST registration structure; check character confirmed against published registrations",
      "Election Commission of India, Manual on Electoral Rolls",
      "No official specification found; format as used by Microsoft Presidio",
      "Department of Telecommunications, National Numbering Plan 2003"
    )
  )
}
