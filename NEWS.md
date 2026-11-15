# idcheck (development version)

* New schemes beyond India: `us_ssn` (US Social Security number, by the SSA's
  rules and without placeholders), `gb_nhs` (UK NHS number, modulus 11) and
  `intl_payment_card` (issuer prefix, length and Luhn). Schemes used across
  countries are named `intl_` and have no `country`. IBANs are left to
  'checkstring', as the rules article decided.

# idcheck 0.1.0

* `id_valid()` tests whether strings are well-formed Indian identity numbers:
  Aadhaar (Verhoeff check digit), PAN, GSTIN (Luhn mod 36 check character),
  EPIC, passport and mobile numbers. `id_schemes()` lists them and says which
  are verified by checksum and which by format only.
* The check-digit algorithms are exported on their own, each as a `_valid()`
  and a `_digit()` function: `id_luhn_*()`, `id_verhoeff_*()`, `id_damm_*()`,
  `id_mod11_2_*()` and `id_mod97_10_*()`.
