#include "vctrs.h"
#include "altrep.h"
#include "altrep-rep.h"
#include "altrep-rep-internal.h"

#if !HAS_ALTREP_3_6
// -----------------------------------------------------------------------------
// Non-ALTREP 3.6 support

// [[ include("altrep-rep.h") ]]
bool vec_is_vctrs_compact_rep_lgl(SEXP x) {
  return false;
}

// [[ include("altrep-rep.h") ]]
SEXP new_vctrs_compact_rep_lgl(int value, R_xlen_t size) {
  Rf_errorcall(R_NilValue, "Need R 3.6+ for ALTLOGICAL support");
  return R_NilValue;
}

// [[ register() ]]
SEXP vctrs_new_vctrs_compact_rep_lgl(SEXP value, SEXP size) {
  Rf_errorcall(R_NilValue, "Need R 3.6+ for ALTLOGICAL support");
  return R_NilValue;
}

#else
// -----------------------------------------------------------------------------
// ALTREP implementation

// [[ include("altrep-rep.h") ]]
bool vec_is_vctrs_compact_rep_lgl(SEXP x) {
  if (!ALTREP(x)) {
    return false;
  }

  return ALTREP_CLASS(x) == vctrs_compact_rep_lgl_class_sexp;
}

struct vctrs_compact_rep_lgl_info {
  int value;
  R_xlen_t size;
};

// [[ include("altrep-rep.h") ]]
SEXP new_vctrs_compact_rep_lgl(int value, R_xlen_t size) {
  SEXP info = PROTECT(Rf_allocVector(RAWSXP, sizeof(struct vctrs_compact_rep_lgl_info)));
  struct vctrs_compact_rep_lgl_info* p_info = (struct vctrs_compact_rep_lgl_info*) RAW0(info);

  p_info->value = value;
  p_info->size = size;

  SEXP out = R_new_altrep(vctrs_compact_rep_lgl_class, info, R_NilValue);

  // Force duplicate on modify
  MARK_NOT_MUTABLE(out);

  UNPROTECT(1);
  return out;
}

// [[ register() ]]
SEXP vctrs_new_vctrs_compact_rep_lgl(SEXP value, SEXP size) {
  int value_ = LOGICAL(value)[0];
  R_xlen_t size_ = (R_xlen_t) REAL(size)[0];

  return new_vctrs_compact_rep_lgl(value_, size_);
}

// -----------------------------------------------------------------------------

#define VCTRS_COMPACT_REP_LGL_VALUE(info) (((struct vctrs_compact_rep_lgl_info*) RAW0(info))->value)
#define VCTRS_COMPACT_REP_LGL_SIZE(info) (((struct vctrs_compact_rep_lgl_info*) RAW0(info))->size)

// Materialize the full vector
static SEXP vctrs_compact_rep_lgl_materialize(SEXP x) {
  SEXP info = VCTRS_COMPACT_REP_INFO(x);
  int value = VCTRS_COMPACT_REP_LGL_VALUE(info);
  R_xlen_t size = VCTRS_COMPACT_REP_LGL_SIZE(info);

  SEXP out = PROTECT(Rf_allocVector(LGLSXP, size));
  int* p_out = LOGICAL(out);

  for (R_xlen_t i = 0; i < size; ++i) {
    p_out[i] = value;
  }

  UNPROTECT(1);
  return out;
}

static SEXP vctrs_compact_rep_lgl_Serialized_state(SEXP x) {
  return VCTRS_COMPACT_REP_INFO(x);
}

static SEXP vctrs_compact_rep_lgl_Unserialize(SEXP cls, SEXP state) {
  SEXP info = state;
  int value = VCTRS_COMPACT_REP_LGL_VALUE(info);
  R_xlen_t size = VCTRS_COMPACT_REP_LGL_SIZE(info);

  return new_vctrs_compact_rep_lgl(value, size);
}

// TODO: What if `deep = false`? vroom dttm duplicates the altrep object
// but compact_intseq objects always materialize
static SEXP vctrs_compact_rep_lgl_Duplicate(SEXP x, Rboolean deep) {
  return vctrs_compact_rep_lgl_materialize(x);
}

// Drop through to standard coercion methods for now.
// We could coerce from one compact rep type to another.
static SEXP vctrs_compact_rep_lgl_Coerce(SEXP x, int type) {
  return NULL;
}

static Rboolean vctrs_compact_rep_lgl_Inspect(SEXP x,
                                              int pre,
                                              int deep,
                                              int pvec,
                                              void (*inspect_subtree)(SEXP, int, int, int)) {
  SEXP info = VCTRS_COMPACT_REP_INFO(x);
  int value = VCTRS_COMPACT_REP_LGL_VALUE(info);
  R_xlen_t size = VCTRS_COMPACT_REP_LGL_SIZE(info);
  const char* state = VCTRS_COMPACT_REP_IS_COMPACT(x) ? "compact" : "expanded";

  Rprintf("vctrs_compact_rep_lgl (value: %i, size: %td, state: %s)", value, size, state);
  Rprintf("\n");

  return TRUE;
}

static R_xlen_t vctrs_compact_rep_lgl_Length(SEXP x) {
  SEXP info = VCTRS_COMPACT_REP_INFO(x);
  return VCTRS_COMPACT_REP_LGL_SIZE(info);
}

static void* vctrs_compact_rep_lgl_Dataptr(SEXP x, Rboolean writeable) {
  if (VCTRS_COMPACT_REP_IS_COMPACT(x)) {
    VCTRS_COMPACT_REP_SET_DATA(x, vctrs_compact_rep_lgl_materialize(x));
  }

  return DATAPTR(VCTRS_COMPACT_REP_DATA(x));
}

static const void* vctrs_compact_rep_lgl_Dataptr_or_null(SEXP x) {
  if (VCTRS_COMPACT_REP_IS_COMPACT(x)) {
    return NULL;
  } else {
    return vctrs_compact_rep_lgl_Dataptr(x, FALSE);
  }
}

#define COMPACT_REP_LGL_EXTRACT_SUBSET_LOOP(CTYPE, CONST_DEREF, LOC_IS_FINITE) do { \
  const CTYPE* p_indx = CONST_DEREF(indx);                                          \
                                                                                    \
  for (R_xlen_t i = 0; i < out_size; ++i) {                                         \
    const CTYPE loc = p_indx[i];                                                    \
                                                                                    \
    if (LOC_IS_FINITE && 0 < loc && loc <= size) {                                  \
      p_out[i] = value;                                                             \
    } else {                                                                        \
      p_out[i] = NA_LOGICAL;                                                        \
    }                                                                               \
  }                                                                                 \
} while(0)

static SEXP vctrs_compact_rep_lgl_Extract_subset(SEXP x, SEXP indx, SEXP call) {
  const SEXP info = VCTRS_COMPACT_REP_INFO(x);
  const int value = VCTRS_COMPACT_REP_LGL_VALUE(info);
  const R_xlen_t size = VCTRS_COMPACT_REP_LGL_SIZE(info);

  const R_xlen_t out_size = Rf_xlength(indx);

  SEXP out = PROTECT(Rf_allocVector(LGLSXP, out_size));
  int* p_out = LOGICAL(out);

  switch (TYPEOF(indx)) {
  case INTSXP: COMPACT_REP_LGL_EXTRACT_SUBSET_LOOP(int, INTEGER_RO, true); break;
  case REALSXP: COMPACT_REP_LGL_EXTRACT_SUBSET_LOOP(double, REAL_RO, R_FINITE(loc)); break;
  }

  UNPROTECT(1);
  return out;
}

#undef COMPACT_REP_LGL_EXTRACT_SUBSET_LOOP

// I believe we should expect that *_ELT() methods will never contain
// an `NA` index. I assumed this from how ExtractSubset() works and from
// how compact_intseq_Elt() is implemented
static int vctrs_compact_rep_lgl_Elt(SEXP x, R_xlen_t i) {
  SEXP info = VCTRS_COMPACT_REP_INFO(x);
  return VCTRS_COMPACT_REP_LGL_VALUE(info);
}

static int vctrs_compact_rep_lgl_No_NA(SEXP x) {
  SEXP info = VCTRS_COMPACT_REP_INFO(x);
  return VCTRS_COMPACT_REP_LGL_VALUE(info) != NA_LOGICAL;
}

static R_xlen_t vctrs_compact_rep_lgl_Get_region(SEXP x, R_xlen_t i, R_xlen_t n, int* buf) {
  SEXP info = VCTRS_COMPACT_REP_INFO(x);
  int value = VCTRS_COMPACT_REP_LGL_VALUE(info);
  R_xlen_t size = VCTRS_COMPACT_REP_LGL_SIZE(info);

  R_xlen_t n_capped = size - i > n ? n : size - i;

  for (R_xlen_t k = 0; k < n_capped; ++k) {
    buf[k] = value;
  }

  return n_capped;
}

#undef VCTRS_COMPACT_REP_LGL_VALUE
#undef VCTRS_COMPACT_REP_LGL_SIZE

// -----------------------------------------------------------------------------

SEXP vctrs_compact_rep_lgl_class_sexp = NULL;
R_altrep_class_t vctrs_compact_rep_lgl_class;

void vctrs_init_vctrs_compact_rep_lgl(DllInfo* dll) {
  vctrs_compact_rep_lgl_class = R_make_altlogical_class("vctrs_compact_rep_lgl", "vctrs", dll);

  vctrs_compact_rep_lgl_class_sexp = R_SEXP(vctrs_compact_rep_lgl_class);
  R_PreserveObject(vctrs_compact_rep_lgl_class_sexp);

  // ALTREP methods
  R_set_altrep_Serialized_state_method(vctrs_compact_rep_lgl_class, vctrs_compact_rep_lgl_Serialized_state);
  R_set_altrep_Unserialize_method(vctrs_compact_rep_lgl_class, vctrs_compact_rep_lgl_Unserialize);
  R_set_altrep_Duplicate_method(vctrs_compact_rep_lgl_class, vctrs_compact_rep_lgl_Duplicate);
  R_set_altrep_Coerce_method(vctrs_compact_rep_lgl_class, vctrs_compact_rep_lgl_Coerce);
  R_set_altrep_Inspect_method(vctrs_compact_rep_lgl_class, vctrs_compact_rep_lgl_Inspect);
  R_set_altrep_Length_method(vctrs_compact_rep_lgl_class, vctrs_compact_rep_lgl_Length);

  // ALTVEC methods
  R_set_altvec_Dataptr_method(vctrs_compact_rep_lgl_class, vctrs_compact_rep_lgl_Dataptr);
  R_set_altvec_Dataptr_or_null_method(vctrs_compact_rep_lgl_class, vctrs_compact_rep_lgl_Dataptr_or_null);
  R_set_altvec_Extract_subset_method(vctrs_compact_rep_lgl_class, vctrs_compact_rep_lgl_Extract_subset);

  // ALTLOGICAL methods
  R_set_altlogical_Elt_method(vctrs_compact_rep_lgl_class, vctrs_compact_rep_lgl_Elt);
  R_set_altlogical_No_NA_method(vctrs_compact_rep_lgl_class, vctrs_compact_rep_lgl_No_NA);
  R_set_altlogical_Get_region_method(vctrs_compact_rep_lgl_class, vctrs_compact_rep_lgl_Get_region);
}

#endif