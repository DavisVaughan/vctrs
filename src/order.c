#include "vctrs.h"

// -----------------------------------------------------------------------------

#define HEX_UINT32_SIGN_BIT 0x80000000u

// [INT32_MIN, INT32_MAX] => [0, UINT32_MAX]
static inline uint32_t map_from_int32_to_uint32(int32_t x) {
  return x ^ HEX_UINT32_SIGN_BIT;
}

#undef HEX_UINT32_SIGN_BIT

// -----------------------------------------------------------------------------

static inline uint8_t extract_byte(uint32_t x, uint8_t pass) {
  return (x >> (8 * pass)) & UINT8_MAX;
}

// -----------------------------------------------------------------------------

#define UINT8_MAX_SIZE (UINT8_MAX + 1)
#define PASS_OFFSET(size, pass) (size * (R_xlen_t) pass)

SEXP vctrs_int_radix_sort(SEXP x) {
  const uint8_t n_passes = 4;

  const int* p_x = INTEGER_RO(x);

  const R_xlen_t size = Rf_xlength(x);

  R_xlen_t* p_counts = (R_xlen_t*) R_alloc(UINT8_MAX_SIZE * n_passes, sizeof(R_xlen_t));
  memset(p_counts, 0, UINT8_MAX_SIZE * n_passes * sizeof(R_xlen_t));

  //uint8_t* p_bytes = (uint8_t*) R_alloc(size * n_passes, sizeof(uint8_t));

  // Build 4 histograms in one pass (one for each byte)
  for (R_xlen_t i = 0; i < size; ++i) {
    const int32_t elt_x = p_x[i];
    const uint32_t elt_mapped = map_from_int32_to_uint32(elt_x);

    for (uint8_t pass = 0; pass < n_passes; ++pass) {
      const uint8_t byte = extract_byte(elt_mapped, pass);
      //p_bytes[i + PASS_OFFSET(size, pass)] = byte;
      p_counts[byte + PASS_OFFSET(UINT8_MAX_SIZE, pass)]++;
    }
  }

  SEXP out = PROTECT(Rf_duplicate(x));
  int* p_out = INTEGER(out);

  SEXP copy = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_copy = INTEGER(copy);

  R_xlen_t p_offsets[UINT8_MAX_SIZE];

  // TODO: Maybe not required
  memset(p_offsets, 0, UINT8_MAX_SIZE * sizeof(R_xlen_t));

  for (uint8_t pass = 0; pass < n_passes; ++pass) {
    // Load `copy` with current copy of `out`
    for (R_xlen_t i = 0; i < size; ++i) {
      p_copy[i] = p_out[i];
    }

    const R_xlen_t counts_offset = PASS_OFFSET(UINT8_MAX_SIZE, pass);

    R_xlen_t offset = 0;

    for (R_xlen_t i = 0; i < UINT8_MAX_SIZE; ++i) {
      p_offsets[i] = offset;
      offset += p_counts[i + counts_offset];
    }

    for (R_xlen_t i = 0; i < size; ++i) {
      const int32_t elt_copy = p_copy[i];
      const uint32_t elt_mapped = map_from_int32_to_uint32(elt_copy);
      const uint8_t loc = extract_byte(elt_mapped, pass);

      p_out[p_offsets[loc]++] = elt_copy;
    }
  }

  UNPROTECT(2);
  return out;
}

#undef PASS_OFFSET
