#include "vctrs.h"
#include "utils.h"


// Defined below
static enum vctrs_class_type class_type_impl(SEXP class);
static const char* class_type_as_str(enum vctrs_class_type type);


// [[ register() ]]
SEXP vctrs_class_type(SEXP x) {
  return Rf_mkString(class_type_as_str(class_type(x)));
}

// [[ include("utils.h") ]]
bool is_data_frame(SEXP x) {
  enum vctrs_class_type type = class_type(x);
  return
    type == vctrs_class_bare_data_frame ||
    type == vctrs_class_bare_tibble ||
    type == vctrs_class_data_frame;
}

// [[ include("utils.h") ]]
bool is_native_df(SEXP x) {
  enum vctrs_class_type type = class_type(x);
  return
    type == vctrs_class_bare_data_frame ||
    type == vctrs_class_bare_tibble;
}

// [[ include("utils.h") ]]
bool is_bare_data_frame(SEXP x) {
  return class_type(x) == vctrs_class_bare_data_frame;
}

// [[ include("utils.h") ]]
bool is_bare_tibble(SEXP x) {
  return class_type(x) == vctrs_class_bare_tibble;
}

// [[ include("utils.h") ]]
bool is_record(SEXP x) {
  enum vctrs_class_type type = class_type(x);
  return
    type == vctrs_class_rcrd ||
    type == vctrs_class_posixlt;
}


enum vctrs_class_type class_type(SEXP x) {
  if (!OBJECT(x)) {
    return vctrs_class_none;
  }

  SEXP class = PROTECT(Rf_getAttrib(x, R_ClassSymbol));
  enum vctrs_class_type type = class_type_impl(class);

  UNPROTECT(1);
  return type;
}

static enum vctrs_class_type class_type_impl(SEXP class) {
  int n = Rf_length(class);
  SEXP const* p = STRING_PTR(class);

  // First check for bare types for which we know how many strings are
  // the classes composed of
  switch (n) {
  case 1: {
    if (p[0] == strings_data_frame) {
      return vctrs_class_bare_data_frame;
    } else if (p[0] == strings_factor) {
      return vctrs_class_bare_factor;
    }
    break;
  }
  case 2: {
    if (p[0] == strings_ordered &&
        p[1] == strings_factor) {
      return vctrs_class_bare_ordered;
    }
    break;
  }
  case 3: {
    if (p[0] == strings_tbl_df &&
        p[1] == strings_tbl &&
        p[2] == strings_data_frame) {
      return vctrs_class_bare_tibble;
    }
    break;
  }
  }

  // Now check for inherited classes
  SEXP butlast = p[n - 2];
  SEXP last = p[n - 1];

  if (last == strings_data_frame) {
    return vctrs_class_data_frame;
  } else if (last == strings_posixt && butlast == strings_posixlt) {
    return vctrs_class_posixlt;
  } else if (last == strings_vctrs_vctr && butlast == strings_vctrs_rcrd) {
    return vctrs_class_rcrd;
  } else if (last == strings_factor) {
    if (butlast == strings_ordered) {
      return vctrs_class_ordered;
    } else {
      return vctrs_class_factor;
    }
  }

  return vctrs_class_unknown;
}

static const char* class_type_as_str(enum vctrs_class_type type) {
  switch (type) {
  case vctrs_class_data_frame: return "data_frame";
  case vctrs_class_bare_data_frame: return "bare_data_frame";
  case vctrs_class_bare_tibble: return "bare_tibble";
  case vctrs_class_rcrd: return "rcrd";
  case vctrs_class_posixlt: return "posixlt";
  case vctrs_class_factor: return "factor";
  case vctrs_class_bare_factor: return "bare_factor";
  case vctrs_class_ordered: return "ordered";
  case vctrs_class_bare_ordered: return "bare_ordered";
  case vctrs_class_unknown: return "unknown";
  case vctrs_class_none: return "none";
  }
  never_reached("class_type_as_str");
}


// [[ include("vctrs.h") ]]
bool vec_is_partial(SEXP x) {
  return x == R_NilValue || Rf_inherits(x, "vctrs_partial");
}
