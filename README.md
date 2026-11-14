# idcheck

<!-- badges: start -->
[![R-CMD-check](https://github.com/gojiplus/idcheck/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/gojiplus/idcheck/actions/workflows/R-CMD-check.yml)
<!-- badges: end -->

Is this string a valid identity number? `idcheck` answers for a whole
character vector at once, checks the check digit where the scheme has one, and
returns `FALSE` for junk instead of stopping.

The first release covers India. A `TRUE` means the string is well formed. It
does not mean the number was issued, or to whom.

## Installation

```r
# install.packages("pak")
pak::pak("gojiplus/idcheck")
```

## Identity numbers

```r
library(idcheck)

# A test number published by UIDAI; the same with its last digit changed;
# twelve arbitrary digits; a missing value
id_valid(c("9999 4105 7058", "9999 4105 7059", "1234 5678 9012", NA), "in_aadhaar")
#> [1]  TRUE FALSE FALSE    NA

# The fourth character of a PAN says what kind of holder it is; D is no kind
id_valid(c("AAAPL1234C", "ABCDE1234F"), "in_pan")
#> [1]  TRUE FALSE

# The GSTIN that guides print as a placeholder ends in the wrong check character
id_valid(c("22AAAAA0000A1ZC", "22AAAAA0000A1Z5"), "in_gstin")
#> [1]  TRUE FALSE

id_valid(c("+91 98765 43210", "12345 67890"), "in_mobile")
#> [1]  TRUE FALSE
```

Spaces and hyphens are ignored and letters are upper-cased before a value is
tested.

`id_schemes()` lists what is covered and how strong each test is. A scheme
marked `format` has no public check digit, so many arbitrary strings of the
right shape will pass it; a scheme marked `checksum` rejects nine in ten of
them (35 in 36 for GSTIN).

```r
id_schemes()[, c("scheme", "name", "check", "algorithm")]
#>        scheme                                                 name    check   algorithm
#> 1  in_aadhaar                                       Aadhaar number checksum    Verhoeff
#> 2      in_pan                       Permanent Account Number (PAN)   format        <NA>
#> 3    in_gstin Goods and Services Tax Identification Number (GSTIN) checksum Luhn mod 36
#> 4     in_epic          Elector's Photo Identity Card (EPIC) number   format        <NA>
#> 5 in_passport                                      Passport number   format        <NA>
#> 6   in_mobile                                  Mobile phone number   format        <NA>
```

To ask what a column looks like, take the share that passes each scheme:

```r
x <- c("999941057058", "999971658847", "999933119405", "999955183433", "n/a")
vapply(id_schemes()$scheme, function(s) mean(id_valid(x, s)), numeric(1))
#>  in_aadhaar      in_pan    in_gstin     in_epic in_passport   in_mobile
#>         0.8         0.0         0.0         0.0         0.0         0.0
```

## Check-digit algorithms

Each algorithm has a `_valid()` function for a number that carries its check
digit and a `_digit()` function that computes one. These take the bare string:
no separators are stripped.

```r
id_verhoeff_digit("236")
#> [1] "3"
id_verhoeff_valid(c("2363", "2364", "3263"))
#> [1]  TRUE FALSE FALSE

id_luhn_valid("4111111111111111")
#> [1] TRUE
id_damm_digit("572")
#> [1] "4"
id_mod11_2_digit("000000021825009")   # ISO 7064 MOD 11-2, as in ORCID
#> [1] "7"
id_mod97_10_digits("794")             # ISO 7064 MOD 97-10, as in IBAN
#> [1] "44"
```

## Speed

The loops are C++. On an Apple M4, one million 12-character strings take about
0.07 seconds through `id_valid(x, "in_aadhaar")` and 0.04 seconds through
`id_verhoeff_valid(x)`. The pure-R `CheckDigit::VerifyCheckDigit()` takes 14
seconds on the same vector and returns the same answers.

## What it rests on

[SPEC.md](https://github.com/gojiplus/idcheck/blob/main/SPEC.md) gives the
source for every rule and says where one could not be confirmed. Two things to
know before relying on a result:

- The GSTIN check character is computed by Luhn mod 36. No official document
  stating that was found; the algorithm was confirmed by reproducing the check
  character of 17 published company registrations.
- PAN, EPIC and passport numbers have check digits or internal rules that are
  not public. Only their shape is tested.

## Related packages

- [`checkstring`](https://cran.r-project.org/package=checkstring) validates
  IBAN, LEI, ISIN, ISBN, ORCID and other non-national identifiers.
- [`CheckDigit`](https://cran.r-project.org/package=CheckDigit) and
  [`verhoeff`](https://cran.r-project.org/package=verhoeff) implement Verhoeff
  in R; [`checkLuhn`](https://cran.r-project.org/package=checkLuhn) implements
  Luhn.

## License

MIT
