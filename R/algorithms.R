#' Luhn check digit
#'
#' @description
#' The Luhn (mod 10) algorithm guards payment card numbers, IMEIs and many
#' national identifiers. It catches every single-digit error and every swap of
#' two neighboring digits except `09` with `90`.
#'
#' `id_luhn_valid()` tests a number that ends in its check digit.
#' `id_luhn_digit()` computes the check digit for a number that lacks one.
#'
#' @param x A character vector of digit strings, with no spaces or other
#'   separators.
#'
#' @returns
#' `id_luhn_valid()` returns a logical vector: `TRUE` where the string is two
#' or more digits and the check passes, `FALSE` otherwise, `NA` where `x` is
#' `NA`.
#'
#' `id_luhn_digit()` returns a character vector holding the check digit, or
#' `NA` where `x` is `NA` or is not a string of one or more digits.
#'
#' @references ISO/IEC 7812-1, *Identification cards: Identification of
#'   issuers*, Annex B.
#' @family check-digit algorithms
#' @export
#' @examples
#' id_luhn_digit("7992739871")
#' id_luhn_valid(c("79927398713", "79927398710", "7992 7398 713", NA))
id_luhn_valid <- function(x) {
  algorithm_valid(x, "luhn")
}

#' @rdname id_luhn_valid
#' @export
id_luhn_digit <- function(x) {
  algorithm_digit(x, "luhn")
}

#' Verhoeff check digit
#'
#' @description
#' Verhoeff's algorithm works in the dihedral group of order 10 rather than
#' with ordinary addition, which lets it catch every single-digit error and
#' every swap of two neighboring digits. Aadhaar numbers use it.
#'
#' `id_verhoeff_valid()` tests a number that ends in its check digit.
#' `id_verhoeff_digit()` computes the check digit for a number that lacks one.
#'
#' @inheritParams id_luhn_valid
#'
#' @returns
#' `id_verhoeff_valid()` returns a logical vector: `TRUE` where the string is
#' two or more digits and the check passes, `FALSE` otherwise, `NA` where `x`
#' is `NA`.
#'
#' `id_verhoeff_digit()` returns a character vector holding the check digit,
#' or `NA` where `x` is `NA` or is not a string of one or more digits.
#'
#' @references Verhoeff, J. (1969). *Error Detecting Decimal Codes*.
#'   Mathematical Centre Tract 29. Amsterdam: Mathematisch Centrum.
#' @family check-digit algorithms
#' @export
#' @examples
#' id_verhoeff_digit(c("236", "12345"))
#' id_verhoeff_valid(c("2363", "2364", "3263"))
id_verhoeff_valid <- function(x) {
  algorithm_valid(x, "verhoeff")
}

#' @rdname id_verhoeff_valid
#' @export
id_verhoeff_digit <- function(x) {
  algorithm_digit(x, "verhoeff")
}

#' Damm check digit
#'
#' @description
#' Damm's algorithm reads the digits through a single 10 by 10 table (a
#' totally anti-symmetric quasigroup). Like Verhoeff's, it catches every
#' single-digit error and every swap of two neighboring digits.
#'
#' `id_damm_valid()` tests a number that ends in its check digit.
#' `id_damm_digit()` computes the check digit for a number that lacks one.
#'
#' @inheritParams id_luhn_valid
#'
#' @returns
#' `id_damm_valid()` returns a logical vector: `TRUE` where the string is two
#' or more digits and the check passes, `FALSE` otherwise, `NA` where `x` is
#' `NA`.
#'
#' `id_damm_digit()` returns a character vector holding the check digit, or
#' `NA` where `x` is `NA` or is not a string of one or more digits.
#'
#' @references Damm, H. M. (2004). *Total anti-symmetrische Quasigruppen*.
#'   Dissertation, Philipps-Universität Marburg.
#' @family check-digit algorithms
#' @export
#' @examples
#' id_damm_digit("572")
#' id_damm_valid(c("5724", "5274"))
id_damm_valid <- function(x) {
  algorithm_valid(x, "damm")
}

#' @rdname id_damm_valid
#' @export
id_damm_digit <- function(x) {
  algorithm_digit(x, "damm")
}

#' ISO 7064 MOD 11-2 check character
#'
#' @description
#' One check character, a digit or `X` (standing for 10), computed modulo 11
#' with weights that are powers of 2. ORCID identifiers and the International
#' Standard Name Identifier use it.
#'
#' `id_mod11_2_valid()` tests a number that ends in its check character.
#' `id_mod11_2_digit()` computes the check character for a number that lacks
#' one.
#'
#' @inheritParams id_luhn_valid
#'
#' @returns
#' `id_mod11_2_valid()` returns a logical vector: `TRUE` where the string is
#' one or more digits followed by a digit or an upper-case `X` and the check
#' passes, `FALSE` otherwise, `NA` where `x` is `NA`.
#'
#' `id_mod11_2_digit()` returns a character vector holding the check
#' character, `"0"` to `"9"` or `"X"`, or `NA` where `x` is `NA` or is not a
#' string of one or more digits.
#'
#' @references ISO/IEC 7064:2003, *Information technology: Security
#'   techniques: Check character systems*.
#' @family check-digit algorithms
#' @export
#' @examples
#' # The sample identifier in the ORCID documentation, without its hyphens
#' id_mod11_2_digit("000000021825009")
#' id_mod11_2_valid(c("0000000218250097", "0000000218250098"))
id_mod11_2_valid <- function(x) {
  algorithm_valid(x, "mod11_2")
}

#' @rdname id_mod11_2_valid
#' @export
id_mod11_2_digit <- function(x) {
  algorithm_digit(x, "mod11_2")
}

#' ISO 7064 MOD 97-10 check digits
#'
#' @description
#' Two check digits chosen so that the whole number leaves remainder 1 when
#' divided by 97. Bank account numbers (IBAN) and Legal Entity Identifiers
#' use it. Letters are read as two-digit numbers, `A` as 10 through `Z` as 35,
#' which is how those standards apply it.
#'
#' `id_mod97_10_valid()` tests a string that ends in its two check digits.
#' `id_mod97_10_digits()` computes the check digits for a string that lacks
#' them.
#'
#' An IBAN keeps its check digits in positions 3 and 4. Move the first four
#' characters to the end before testing it; the package `checkstring` does
#' this for you.
#'
#' @param x A character vector of strings made of digits and upper-case
#'   letters, with no spaces or other separators.
#'
#' @returns
#' `id_mod97_10_valid()` returns a logical vector: `TRUE` where the string is
#' made of digits and upper-case letters, ends in two digits and passes the
#' check, `FALSE` otherwise, `NA` where `x` is `NA`.
#'
#' `id_mod97_10_digits()` returns a character vector holding the two check
#' digits, `"02"` to `"98"`, or `NA` where `x` is `NA` or holds anything but
#' digits and upper-case letters.
#'
#' @references ISO/IEC 7064:2003, *Information technology: Security
#'   techniques: Check character systems*.
#' @family check-digit algorithms
#' @export
#' @examples
#' id_mod97_10_digits("794")
#' id_mod97_10_valid(c("79444", "79445"))
#'
#' # The example IBAN GB82 WEST 1234 5698 7654 32, rearranged
#' id_mod97_10_valid("WEST12345698765432GB82")
id_mod97_10_valid <- function(x) {
  algorithm_valid(x, "mod97_10")
}

#' @rdname id_mod97_10_valid
#' @export
id_mod97_10_digits <- function(x) {
  algorithm_digit(x, "mod97_10")
}
