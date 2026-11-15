# Positions in these vectors are the codes the C++ dispatch tables in
# src/idcheck.cpp are indexed by, so the two must keep the same order.
algorithm_names <- c("luhn", "verhoeff", "damm", "mod11_2", "mod97_10")
scheme_names <- c(
  "in_aadhaar", "in_pan", "in_gstin", "in_epic", "in_passport", "in_mobile",
  "us_ssn", "gb_nhs", "intl_iban", "intl_payment_card"
)

algorithm_code <- function(name) {
  match(name, algorithm_names) - 1L
}

check_character <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (!is.character(x)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a character vector, not {.obj_type_friendly {x}}.",
        i = "Numbers stored as numeric have already lost leading zeros; read them as text."
      ),
      call = call
    )
  }
}

algorithm_valid <- function(x, name, call = rlang::caller_env()) {
  check_character(x, arg = "x", call = call)
  algorithm_valid_(x, algorithm_code(name))
}

algorithm_digit <- function(x, name, call = rlang::caller_env()) {
  check_character(x, arg = "x", call = call)
  algorithm_digit_(x, algorithm_code(name))
}
