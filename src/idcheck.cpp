#include <cpp11.hpp>

#include <cstring>

namespace {

// Cayley table of the dihedral group D5, the permutation applied by position,
// and inverses, from Verhoeff (1969). tests/testthat/helper-reference.R derives
// the same tables from the group definition and checks this code against them.
const unsigned char kVerhoeffD[10][10] = {
    {0, 1, 2, 3, 4, 5, 6, 7, 8, 9}, {1, 2, 3, 4, 0, 6, 7, 8, 9, 5},
    {2, 3, 4, 0, 1, 7, 8, 9, 5, 6}, {3, 4, 0, 1, 2, 8, 9, 5, 6, 7},
    {4, 0, 1, 2, 3, 9, 5, 6, 7, 8}, {5, 9, 8, 7, 6, 0, 4, 3, 2, 1},
    {6, 5, 9, 8, 7, 1, 0, 4, 3, 2}, {7, 6, 5, 9, 8, 2, 1, 0, 4, 3},
    {8, 7, 6, 5, 9, 3, 2, 1, 0, 4}, {9, 8, 7, 6, 5, 4, 3, 2, 1, 0}};

const unsigned char kVerhoeffP[8][10] = {
    {0, 1, 2, 3, 4, 5, 6, 7, 8, 9}, {1, 5, 7, 6, 2, 8, 3, 0, 9, 4},
    {5, 8, 0, 3, 7, 9, 6, 1, 4, 2}, {8, 9, 1, 6, 0, 4, 3, 5, 2, 7},
    {9, 4, 5, 3, 1, 2, 6, 8, 7, 0}, {4, 2, 8, 6, 5, 7, 3, 9, 0, 1},
    {2, 7, 9, 3, 8, 0, 6, 4, 1, 5}, {7, 0, 4, 6, 9, 1, 3, 2, 5, 8}};

const unsigned char kVerhoeffInv[10] = {0, 4, 3, 2, 1, 5, 6, 7, 8, 9};

// The order-10 totally anti-symmetric quasigroup from Damm (2004).
const unsigned char kDamm[10][10] = {
    {0, 3, 1, 7, 5, 9, 8, 6, 4, 2}, {7, 0, 9, 2, 1, 5, 4, 8, 6, 3},
    {4, 2, 0, 6, 8, 7, 1, 3, 5, 9}, {1, 7, 5, 0, 9, 8, 3, 4, 2, 6},
    {6, 1, 2, 3, 0, 4, 5, 9, 7, 8}, {3, 6, 7, 4, 2, 0, 9, 5, 8, 1},
    {5, 8, 6, 9, 7, 2, 0, 1, 3, 4}, {8, 9, 4, 5, 3, 6, 2, 0, 1, 7},
    {9, 4, 3, 8, 6, 1, 7, 2, 0, 5}, {2, 5, 8, 1, 4, 3, 6, 7, 9, 0}};

inline bool is_digit(char c) { return c >= '0' && c <= '9'; }
inline bool is_upper(char c) { return c >= 'A' && c <= 'Z'; }

// 0-9 then A-Z as 10-35; -1 for anything else.
inline int value36(char c) {
  if (is_digit(c)) return c - '0';
  if (is_upper(c)) return c - 'A' + 10;
  return -1;
}

bool all_digits(const char* s, R_xlen_t n) {
  for (R_xlen_t i = 0; i < n; ++i) {
    if (!is_digit(s[i])) return false;
  }
  return true;
}

// The *_digit functions return -1 where the algorithm is undefined.

// Doubling a digit and adding the digits of the result.
const unsigned char kLuhnDouble[10] = {0, 2, 4, 6, 8, 1, 3, 5, 7, 9};

// `double_last` says whether the rightmost digit is one of the doubled ones:
// no when it is the check digit, yes when the check digit is still to come.
// Returns the sum modulo 10, or -1 on a character that is not a digit.
int luhn_sum(const char* s, R_xlen_t n, bool double_last) {
  unsigned long long sum = 0;
  bool doubled = double_last;
  for (R_xlen_t i = n; i-- > 0;) {
    if (!is_digit(s[i])) return -1;
    int d = s[i] - '0';
    sum += doubled ? kLuhnDouble[d] : d;
    doubled = !doubled;
  }
  return static_cast<int>(sum % 10);
}

bool luhn_valid(const char* s, R_xlen_t n) {
  return n >= 2 && luhn_sum(s, n, false) == 0;
}

int luhn_digit(const char* s, R_xlen_t n) {
  if (n < 1) return -1;
  int sum = luhn_sum(s, n, true);
  return sum < 0 ? -1 : (10 - sum) % 10;
}

bool verhoeff_valid(const char* s, R_xlen_t n) {
  if (n < 2) return false;
  int c = 0;
  for (R_xlen_t i = 0; i < n; ++i) {
    char ch = s[n - 1 - i];
    if (!is_digit(ch)) return false;
    c = kVerhoeffD[c][kVerhoeffP[i % 8][ch - '0']];
  }
  return c == 0;
}

int verhoeff_digit(const char* s, R_xlen_t n) {
  if (n < 1) return -1;
  int c = 0;
  for (R_xlen_t i = 0; i < n; ++i) {
    char ch = s[n - 1 - i];
    if (!is_digit(ch)) return -1;
    c = kVerhoeffD[c][kVerhoeffP[(i + 1) % 8][ch - '0']];
  }
  return kVerhoeffInv[c];
}

int damm_state(const char* s, R_xlen_t n) {
  int c = 0;
  for (R_xlen_t i = 0; i < n; ++i) {
    if (!is_digit(s[i])) return -1;
    c = kDamm[c][s[i] - '0'];
  }
  return c;
}

bool damm_valid(const char* s, R_xlen_t n) {
  return n >= 2 && damm_state(s, n) == 0;
}

int damm_digit(const char* s, R_xlen_t n) {
  return n < 1 ? -1 : damm_state(s, n);
}

int mod11_2_state(const char* s, R_xlen_t n) {
  int p = 0;
  for (R_xlen_t i = 0; i < n; ++i) {
    if (!is_digit(s[i])) return -1;
    p = ((p + (s[i] - '0')) * 2) % 11;
  }
  return p;
}

bool mod11_2_valid(const char* s, R_xlen_t n) {
  if (n < 2) return false;
  int p = mod11_2_state(s, n - 1);
  if (p < 0) return false;
  char last = s[n - 1];
  int v = last == 'X' ? 10 : (is_digit(last) ? last - '0' : -1);
  return v >= 0 && (p + v) % 11 == 1;
}

// 10 stands for the check character X.
int mod11_2_digit(const char* s, R_xlen_t n) {
  if (n < 1) return -1;
  int p = mod11_2_state(s, n);
  return p < 0 ? -1 : (12 - p) % 11;
}

int mod97_state(const char* s, R_xlen_t n) {
  int p = 0;
  for (R_xlen_t i = 0; i < n; ++i) {
    int v = value36(s[i]);
    if (v < 0) return -1;
    p = v < 10 ? (p * 10 + v) % 97 : (p * 100 + v) % 97;
  }
  return p;
}

bool mod97_10_valid(const char* s, R_xlen_t n) {
  if (n < 3 || !is_digit(s[n - 1]) || !is_digit(s[n - 2])) return false;
  return mod97_state(s, n) == 1;
}

// A value from 2 to 98, written as two digits.
int mod97_10_digits(const char* s, R_xlen_t n) {
  if (n < 1) return -1;
  int p = mod97_state(s, n);
  return p < 0 ? -1 : 98 - (p * 100) % 97;
}

// Luhn mod 36 over the first 14 characters of a GSTIN. -1 on a character
// outside 0-9A-Z.
int luhn36_digit(const char* s, int n) {
  int sum = 0;
  for (int i = 0; i < n; ++i) {
    int v = value36(s[n - 1 - i]);
    if (v < 0) return -1;
    int p = i % 2 == 0 ? v * 2 : v;
    sum += p / 36 + p % 36;
  }
  return (36 - sum % 36) % 36;
}

const int kBufferSize = 32;

// Copies s without ASCII whitespace and hyphens, upper-casing letters, so
// "9999 4105 7058" and "abcpe1234f" are judged as written on the card.
// Returns the new length, or -1 if it exceeds the buffer: no scheme is that
// long, so the value is invalid whatever it holds.
int normalize(const char* s, R_xlen_t n, char* out) {
  int m = 0;
  for (R_xlen_t i = 0; i < n; ++i) {
    char c = s[i];
    if (c == ' ' || c == '-' || c == '\t' || c == '\n' || c == '\r') continue;
    if (m == kBufferSize) return -1;
    out[m++] = (c >= 'a' && c <= 'z') ? static_cast<char>(c - 'a' + 'A') : c;
  }
  return m;
}

bool pan_format(const char* b) {
  for (int i = 0; i < 5; ++i) {
    if (!is_upper(b[i])) return false;
  }
  switch (b[3]) {
    case 'A': case 'B': case 'C': case 'F': case 'G':
    case 'H': case 'J': case 'L': case 'P': case 'T':
      break;
    default:
      return false;
  }
  return all_digits(b + 5, 4) && is_upper(b[9]);
}

bool in_aadhaar(const char* b, int m) {
  return m == 12 && b[0] >= '2' && b[0] <= '9' && verhoeff_valid(b, m);
}

bool in_pan(const char* b, int m) { return m == 10 && pan_format(b); }

bool in_gstin(const char* b, int m) {
  if (m != 15 || !is_digit(b[0]) || !is_digit(b[1])) return false;
  int state = (b[0] - '0') * 10 + (b[1] - '0');
  if (!((state >= 1 && state <= 38) || state == 97)) return false;
  if (!pan_format(b + 2)) return false;
  if (!(is_upper(b[12]) || (b[12] >= '1' && b[12] <= '9'))) return false;
  if (b[13] != 'Z' && b[13] != 'C' && b[13] != 'D') return false;
  int check = luhn36_digit(b, 14);
  return check >= 0 && check == value36(b[14]);
}

bool in_epic(const char* b, int m) {
  return m == 10 && is_upper(b[0]) && is_upper(b[1]) && is_upper(b[2]) &&
         all_digits(b + 3, 7);
}

bool in_passport(const char* b, int m) {
  return m == 8 && is_upper(b[0]) && all_digits(b + 1, 7);
}

bool in_mobile(const char* b, int m) {
  if (m == 13 && b[0] == '+' && b[1] == '9' && b[2] == '1') {
    b += 3;
  } else if (m == 12 && b[0] == '9' && b[1] == '1') {
    b += 2;
  } else if (m == 11 && b[0] == '0') {
    b += 1;
  } else if (m != 10) {
    return false;
  }
  return b[0] >= '6' && b[0] <= '9' && all_digits(b, 10);
}

// Social Security Administration: no area 000, 666 or 900-999 (the 9xx
// range is for ITINs), no group 00, no serial 0000. Following Microsoft
// Presidio's recognizer, also not a single repeated digit or a published
// placeholder (123-45-6789, 987-65-4320, 078-05-1120), which in data are
// almost always dummies.
bool us_ssn(const char* b, int m) {
  if (m != 9 || !all_digits(b, 9)) return false;
  int area = (b[0] - '0') * 100 + (b[1] - '0') * 10 + (b[2] - '0');
  int group = (b[3] - '0') * 10 + (b[4] - '0');
  bool serial = b[5] != '0' || b[6] != '0' || b[7] != '0' || b[8] != '0';
  if (area == 0 || area == 666 || area >= 900 || group == 0 || !serial) return false;
  bool repeated = true;
  for (int i = 1; i < 9; ++i) repeated = repeated && b[i] == b[0];
  return !repeated && std::strncmp(b, "123456789", 9) != 0 &&
         std::strncmp(b, "987654320", 9) != 0 && std::strncmp(b, "078051120", 9) != 0;
}

// NHS Data Dictionary, NHS Number: ten digits, the last a modulus 11 check
// digit over the first nine weighted 10 down to 2; a remainder giving 10 is
// never issued.
bool gb_nhs(const char* b, int m) {
  if (m != 10 || !all_digits(b, 10)) return false;
  int sum = 0;
  for (int i = 0; i < 9; ++i) sum += (10 - i) * (b[i] - '0');
  int check = 11 - sum % 11;
  if (check == 11) check = 0;
  return check != 10 && check == b[9] - '0';
}

bool starts(const char* b, const char* prefix) {
  return std::strncmp(b, prefix, std::strlen(prefix)) == 0;
}

int leading(const char* b, int digits) {
  int v = 0;
  for (int i = 0; i < digits; ++i) v = v * 10 + (b[i] - '0');
  return v;
}

// ISO/IEC 7812: an issuer prefix and a length the issuer uses, then a Luhn
// check digit. Luhn alone passes one random number in ten, these rules about
// three in a hundred. Prefixes and
// lengths for Visa, Mastercard, American Express, Discover, Diners Club,
// JCB, UnionPay, RuPay and Maestro, from the issuer table of Wikipedia's
// "Payment card number" (read 2026-11-14).
bool intl_payment_card(const char* b, int m) {
  if (m < 12 || m > 19 || !all_digits(b, m)) return false;
  int two = leading(b, 2), four = leading(b, 4);
  bool issued =
      (b[0] == '4' && (m == 13 || m == 16 || m == 19)) ||
      (((two >= 51 && two <= 55) || (four >= 2221 && four <= 2720)) && m == 16) ||
      ((two == 34 || two == 37) && m == 15) ||
      ((starts(b, "6011") || two == 65 || (leading(b, 3) >= 644 && leading(b, 3) <= 649)) &&
       m >= 16) ||
      ((two == 30 || two == 36 || two == 38 || two == 39) && m >= 14) ||
      ((four >= 3528 && four <= 3589) && m >= 16) ||
      (two == 62 && m >= 16) ||
      ((two == 60 || two == 81 || two == 82 || starts(b, "508")) && m == 16) ||
      ((four == 5018 || four == 5020 || four == 5038 || four == 5893 || four == 6304 ||
        four == 6759 || (four >= 6761 && four <= 6763)) &&
       m >= 12);
  return issued && luhn_valid(b, m);
}

typedef bool (*valid_fn)(const char*, R_xlen_t);
typedef int (*digit_fn)(const char*, R_xlen_t);
typedef bool (*scheme_fn)(const char*, int);

// The order of each table is the order of the names in R/registry.R.
const valid_fn kValid[] = {luhn_valid, verhoeff_valid, damm_valid,
                           mod11_2_valid, mod97_10_valid};
const digit_fn kDigit[] = {luhn_digit, verhoeff_digit, damm_digit,
                           mod11_2_digit, mod97_10_digits};
const scheme_fn kScheme[] = {in_aadhaar,  in_pan,    in_gstin, in_epic,
                             in_passport, in_mobile, us_ssn,   gb_nhs,
                             intl_payment_card};

template <typename T, int N>
T pick(T const (&table)[N], int code) {
  if (code < 0 || code >= N) cpp11::stop("Internal error: unknown code %d.", code);
  return table[code];
}

}  // namespace

[[cpp11::register]]
cpp11::logicals algorithm_valid_(cpp11::strings x, int algorithm) {
  valid_fn f = pick(kValid, algorithm);
  R_xlen_t n = x.size();
  cpp11::writable::logicals out(n);
  int* po = LOGICAL(out);
  for (R_xlen_t i = 0; i < n; ++i) {
    SEXP s = STRING_ELT(x, i);
    po[i] = s == NA_STRING ? NA_LOGICAL : f(CHAR(s), XLENGTH(s));
  }
  return out;
}

[[cpp11::register]]
cpp11::strings algorithm_digit_(cpp11::strings x, int algorithm) {
  digit_fn f = pick(kDigit, algorithm);
  bool two_digits = f == mod97_10_digits;
  R_xlen_t n = x.size();
  cpp11::writable::strings out(n);
  char buf[2];
  for (R_xlen_t i = 0; i < n; ++i) {
    SEXP s = STRING_ELT(x, i);
    int d = s == NA_STRING ? -1 : f(CHAR(s), XLENGTH(s));
    if (d < 0) {
      SET_STRING_ELT(out, i, NA_STRING);
    } else if (two_digits) {
      buf[0] = static_cast<char>('0' + d / 10);
      buf[1] = static_cast<char>('0' + d % 10);
      SET_STRING_ELT(out, i, Rf_mkCharLen(buf, 2));
    } else {
      buf[0] = d == 10 ? 'X' : static_cast<char>('0' + d);
      SET_STRING_ELT(out, i, Rf_mkCharLen(buf, 1));
    }
  }
  return out;
}

[[cpp11::register]]
cpp11::logicals scheme_valid_(cpp11::strings x, int scheme) {
  scheme_fn f = pick(kScheme, scheme);
  R_xlen_t n = x.size();
  cpp11::writable::logicals out(n);
  int* po = LOGICAL(out);
  char buf[kBufferSize];
  for (R_xlen_t i = 0; i < n; ++i) {
    SEXP s = STRING_ELT(x, i);
    if (s == NA_STRING) {
      po[i] = NA_LOGICAL;
      continue;
    }
    int m = normalize(CHAR(s), XLENGTH(s), buf);
    po[i] = m >= 0 && f(buf, m);
  }
  return out;
}
