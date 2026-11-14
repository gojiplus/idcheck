# Plain R implementations of every rule, written to share as little as possible
# with src/idcheck.cpp. Where an algorithm has more than one formulation, the
# one used here is not the one used there.

digits_of <- function(s) {
  as.integer(strsplit(s, "", fixed = TRUE)[[1]])
}

is_digit_string <- function(s, min_length = 1L) {
  !is.na(s) && grepl(paste0("^[0-9]{", min_length, ",}$"), s)
}

# Verhoeff's tables, generated rather than typed. The digits 0-4 stand for the
# rotations of a pentagon and 5-9 for its reflections; `d` is composition in
# that group. The position permutation is (0 1 5 8 9 4 2 7)(3 6) in cycle
# notation, applied once more for each step to the left.
ref_verhoeff_tables <- function() {
  compose <- function(a, b) {
    if (a < 5 && b < 5) {
      (a + b) %% 5
    } else if (a < 5) {
      5 + (a + b - 5) %% 5
    } else if (b < 5) {
      5 + (a - 5 - b) %% 5
    } else {
      (a - b) %% 5
    }
  }
  d <- outer(0:9, 0:9, Vectorize(compose))

  sigma <- 0:9
  for (cycle in list(c(0, 1, 5, 8, 9, 4, 2, 7), c(3, 6))) {
    sigma[cycle + 1] <- c(cycle[-1], cycle[1])
  }
  p <- matrix(0L, 8, 10)
  p[1, ] <- 0:9
  for (i in 2:8) {
    p[i, ] <- sigma[p[i - 1, ] + 1]
  }

  inv <- apply(d, 1, function(row) which(row == 0) - 1)
  list(d = d, p = p, inv = inv)
}

verhoeff_tables <- ref_verhoeff_tables()

ref_verhoeff_state <- function(dg, offset) {
  t <- verhoeff_tables
  state <- 0
  for (i in seq_along(dg)) {
    pos <- (i - 1 + offset) %% 8
    state <- t$d[state + 1, t$p[pos + 1, dg[i] + 1] + 1]
  }
  state
}

ref_verhoeff_valid <- function(s) {
  if (is.na(s)) {
    return(NA)
  }
  is_digit_string(s, 2L) && ref_verhoeff_state(rev(digits_of(s)), 0) == 0
}

ref_verhoeff_digit <- function(s) {
  if (!is_digit_string(s)) {
    return(NA_character_)
  }
  state <- ref_verhoeff_state(rev(digits_of(s)), 1)
  as.character(verhoeff_tables$inv[state + 1])
}

# Luhn by arithmetic: double every second digit from the right and subtract 9
# from any result above 9.
ref_luhn_sum <- function(dg, double_first) {
  dg <- rev(dg)
  doubled <- (seq_along(dg) %% 2 == 1) == double_first
  twice <- dg * 2
  sum(ifelse(doubled, ifelse(twice > 9, twice - 9, twice), dg))
}

ref_luhn_valid <- function(s) {
  if (is.na(s)) {
    return(NA)
  }
  is_digit_string(s, 2L) && ref_luhn_sum(digits_of(s), FALSE) %% 10 == 0
}

ref_luhn_digit <- function(s) {
  if (!is_digit_string(s)) {
    return(NA_character_)
  }
  as.character((10 - ref_luhn_sum(digits_of(s), TRUE) %% 10) %% 10)
}

# Damm's table, typed a second time from the published one.
damm_table <- matrix(
  digits_of(paste0(
    "0317598642", "7092154863", "4206871359", "1750983426", "6123045978",
    "3674209581", "5869720134", "8945362017", "9438617205", "2581436790"
  )),
  nrow = 10, byrow = TRUE
)

ref_damm_state <- function(dg) {
  Reduce(function(state, digit) damm_table[state + 1, digit + 1], dg, 0)
}

ref_damm_valid <- function(s) {
  if (is.na(s)) {
    return(NA)
  }
  is_digit_string(s, 2L) && ref_damm_state(digits_of(s)) == 0
}

ref_damm_digit <- function(s) {
  if (!is_digit_string(s)) {
    return(NA_character_)
  }
  as.character(ref_damm_state(digits_of(s)))
}

# ISO 7064 MOD 11-2 as the standard states it: the characters, weighted by
# powers of 2 from the right, sum to 1 modulo 11.
pow2_mod11 <- function(k) {
  vapply(k, function(e) Reduce(function(a, b) (a * 2) %% 11, seq_len(e), 1), numeric(1))
}

ref_mod11_2_valid <- function(s) {
  if (is.na(s)) {
    return(NA)
  }
  if (!grepl("^[0-9]+[0-9X]$", s)) {
    return(FALSE)
  }
  ch <- strsplit(s, "", fixed = TRUE)[[1]]
  values <- ifelse(ch == "X", 10, suppressWarnings(as.integer(ch)))
  sum(values * pow2_mod11(rev(seq_along(values)) - 1)) %% 11 == 1
}

ref_mod11_2_digit <- function(s) {
  if (!is_digit_string(s)) {
    return(NA_character_)
  }
  dg <- digits_of(s)
  partial <- sum(dg * pow2_mod11(rev(seq_along(dg)))) %% 11
  c(0:9, "X")[(1 - partial) %% 11 + 1]
}

# ISO 7064 MOD 97-10 by long division in seven-digit chunks, after writing
# each letter as its two-digit number.
ref_mod97_remainder <- function(s) {
  ch <- strsplit(s, "", fixed = TRUE)[[1]]
  expanded <- paste(match(ch, c(0:9, LETTERS)) - 1, collapse = "")
  rem <- 0
  while (nchar(expanded) > 0) {
    chunk <- substr(expanded, 1, 7)
    expanded <- substr(expanded, 8, nchar(expanded))
    rem <- as.numeric(paste0(rem, chunk)) %% 97
  }
  rem
}

ref_mod97_10_valid <- function(s) {
  if (is.na(s)) {
    return(NA)
  }
  grepl("^[0-9A-Z]+[0-9]{2}$", s) && ref_mod97_remainder(s) == 1
}

ref_mod97_10_digits <- function(s) {
  if (is.na(s) || !grepl("^[0-9A-Z]+$", s)) {
    return(NA_character_)
  }
  sprintf("%02d", 98 - ref_mod97_remainder(paste0(s, "00")))
}

# Luhn mod 36, the GSTIN check character.
ref_luhn36_digit <- function(s) {
  values <- rev(match(strsplit(s, "", fixed = TRUE)[[1]], c(0:9, LETTERS)) - 1)
  products <- values * ifelse(seq_along(values) %% 2 == 1, 2, 1)
  total <- sum(products %/% 36 + products %% 36)
  c(0:9, LETTERS)[(36 - total %% 36) %% 36 + 1]
}

# A GSTIN-shaped string with the check character the reference computes.
make_gstin <- function(state, pan, entity = "1", kind = "Z") {
  body <- paste0(state, pan, entity, kind)
  paste0(body, vapply(body, ref_luhn36_digit, character(1)))
}

ref_normalize <- function(x) {
  toupper(gsub("[ \t\n\r-]", "", x))
}

pan_pattern <- "[A-Z]{3}[ABCFGHJLPT][A-Z][0-9]{4}[A-Z]"

ref_scheme_valid <- function(x, scheme) {
  s <- ref_normalize(x)
  out <- switch(scheme,
    in_aadhaar = grepl("^[2-9][0-9]{11}$", s) & vapply(s, ref_verhoeff_valid, logical(1)),
    in_pan = grepl(paste0("^", pan_pattern, "$"), s),
    in_gstin = {
      pattern <- paste0("^(0[1-9]|[12][0-9]|3[0-8]|97)", pan_pattern, "[1-9A-Z][ZCD][0-9A-Z]$")
      shaped <- grepl(pattern, s)
      shaped[shaped] <- vapply(
        s[shaped],
        function(v) ref_luhn36_digit(substr(v, 1, 14)) == substr(v, 15, 15),
        logical(1)
      )
      shaped
    },
    in_epic = grepl("^[A-Z]{3}[0-9]{7}$", s),
    in_passport = grepl("^[A-Z][0-9]{7}$", s),
    in_mobile = grepl("^(\\+91|91|0)?[6-9][0-9]{9}$", s)
  )
  out[is.na(x)] <- NA
  unname(out)
}

# `n` strings over `alphabet`, each of a length drawn from `lengths`.
random_string <- function(n, lengths, alphabet) {
  # Indexing, because sample(10, n) draws from 1:10 rather than returning 10.
  drawn <- lengths[sample.int(length(lengths), n, replace = TRUE)]
  vapply(
    drawn,
    function(k) paste(sample(alphabet, k, replace = TRUE), collapse = ""),
    character(1)
  )
}

random_digits <- function(n, lengths) {
  random_string(n, lengths, 0:9)
}

# Every number one keystroke away from each element of `x`.
single_substitutions <- function(x) {
  unlist(lapply(x, function(s) {
    ch <- strsplit(s, "", fixed = TRUE)[[1]]
    unlist(lapply(seq_along(ch), function(i) {
      vapply(setdiff(as.character(0:9), ch[i]), function(new) {
        ch[i] <- new
        paste(ch, collapse = "")
      }, character(1))
    }))
  }), use.names = FALSE)
}

# Every number made by swapping two unequal neighbors in an element of `x`.
adjacent_transpositions <- function(x) {
  unlist(lapply(x, function(s) {
    ch <- strsplit(s, "", fixed = TRUE)[[1]]
    swappable <- which(ch[-1] != ch[-length(ch)])
    vapply(swappable, function(i) {
      ch[c(i, i + 1)] <- ch[c(i + 1, i)]
      paste(ch, collapse = "")
    }, character(1))
  }), use.names = FALSE)
}

hostile_values <- function() {
  c(
    "", " ", "-", "+", "\t\n", "NA", "1e5", "-123", "12.5", "0x1F",
    strrep("9", 10000), strrep("A", 10000), strrep(" ", 10000),
    "१२३४", "１２３４", "12 34",
    rawToChar(as.raw(c(0xff, 0xfe, 0x31))), "++919876543210", "9876543210+"
  )
}
