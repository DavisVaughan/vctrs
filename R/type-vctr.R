#' vctr (vector) S3 class
#'
#' This abstract class provides a set of useful default methods that makes it
#' considerably easier to get started with a new S3 vector class. See
#' `vignette("s3-vector")` to learn how to use it to create your own S3
#' vector classes.
#'
#' @section Base methods:
#' The vctr class provides methods for many base generics using a smaller
#' set of generics defined by this package. Generally, you should think
#' carefully before overriding any of the methods that vctrs implements for
#' you as they've been carefully planned to be internally consistent.
#'
#' * `[[` and `[` use `NextMethod()` dispatch to the underlying base function,
#'    then restore attributes with `vec_restore()`.
#'    `rep()` and `length<-` work similarly.
#'
#' * `[[<-` and `[<-` cast `value` to same type as `x`, then call
#'   `NextMethod()`.
#'
#' * `as.logical()`, `as.integer()`, `as.numeric()`, `as.character()`,
#'   `as.Date()` and `as.POSIXct()` methods call `vec_cast()`.
#'   The `as.list()` method calls `[[` repeatedly, and the `as.data.frame()`
#'   method uses a standard technique to wrap a vector in a data frame.
#'
#' * `as.factor()`, `as.ordered()` and `as.difftime()` are not generic functions
#'   in base R, but have been reimplemented as generics in the `generics`
#'   package. `vctrs` extends these and calls `vec_cast()`. To inherit this
#'   behavior in a package, import and re-export the generic of interest
#'   from `generics`.
#'
#' * `==`, `!=`, `unique()`, `anyDuplicated()`, and `is.na()` use
#'   [vec_proxy()].
#'
#' * `<`, `<=`, `>=`, `>`, `min()`, `max()`, `range()`, `median()`,
#'   `quantile()`, and `xtfrm()` methods use [vec_proxy_compare()].
#'
#' * `+`, `-`, `/`, `*`, `^`, `%%`, `%/%`, `!`, `&`, and `|` operators
#'   use [vec_arith()].
#'
#' * Mathematical operations including the Summary group generics (`prod()`,
#'   `sum()`, `any()`, `all()`), the Math group generics (`abs()`, `sign()`,
#'   etc), `mean()`, `is.nan()`, `is.finite()`, and `is.infinite()`
#'   use [vec_math()].
#'
#' * `dims()`, `dims<-`, `dimnames()`, `dimnames<-`, `levels()`, and
#'   `levels<-` methods throw errors.
#'
#' @param .data Foundation of class. Must be a vector
#' @param ... Name-value pairs defining attributes
#' @param class Name of subclass.
#' @export
#' @keywords internal
#' @aliases vctr
new_vctr <- function(.data, ..., class = character()) {
  if (!is_vector(.data)) {
    abort("`.data` must be a vector type.")
  }

  nms <- validate_names(.data)
  attrib <- list(names = nms, ..., class = c(class, "vctrs_vctr"))

  .data <- vec_set_attributes(.data, attrib)

  .data <- asS4(.data)

  set_old_class(attrib$class)

  .data
}

# Take care of ensuring the new classes are also S4-able
# (this makes method dispatch work correctly)
# c("x", "vctrs_vctr") will S3 dispatch -> `.vctrs_vctr` method

# We have to be very careful not to try and register the same class signature
# twice. That seems to break things (in particular, method dispatch no longer
# works for subclasses of vctrs_list_of. The polynomial class in the vignette
# broke because of this problem)

# The correct place to "set" the old class into is the vctrs namespace env.
# The information is registered into a `.S3MethodsClasses` table, which is
# created the first time we call `setOldClass()`.

# We only want to unlock/re-lock after the package has been completely loaded
# (which is the point at which it is locked). Otherwise we might prematurely
# re-lock the environment by a call to `new_vctr()`, which happens in the
# testthat helpers and causes problems

# because we are inheriting from "vector" we need
# "Ops" as a "required cached generic". see:
# methods:::.checkRequiredGenerics and
# get(".NeedPrimitiveMethods", envir = .methodsNamespace)

# Set in .onLoad()
ns_vctrs <- NULL

set_old_class <- function(class) {
  if (length(class) == 0L) {
    return()
  }

  class1 <- class[[1L]]
  s3_table <- ns_vctrs$.S3MethodsClasses

  # Don't try and register the class if it already exists in the table
  if (!is.null(s3_table) && vec_in(class1, names(s3_table))) {
    return()
  }

  lock <- FALSE
  if (env_is_locked(ns_vctrs)) {
    lock <- TRUE
    env_unlock(ns_vctrs)
  }

  env_binding_unlock(ns_vctrs, ".requireCachedGenerics")

  methods::setOldClass(class, where = ns_vctrs)

  if (lock) {
    env_lock(ns_vctrs)
  }
}

# Register an S4 class for vctrs_vctr objects that say that they
# "contain" a vector object, allowing us to call `asS4()` on an
# S3 vctrs_vctr

setClass(
  "vctrs_vctr",
  contains = "vector"
)

# Declare the S3 vctrs_vctr class as usable in S4 methods, with
# the above constructor as the one to use
setOldClass(
  "vctrs_vctr",
  S4Class = "vctrs_vctr"
)

#' @importFrom methods show
#' @export
setMethod(
  "show",
  "vctrs_vctr",
  function(object) {
    print(object)
  }
)

#' @export
setMethod(
  "+",
  signature(e1 = "vctrs_vctr", e2 = "ANY"),
  function(e1, e2) {
    vec_arith("+", e1, e2)
  }
)

#' @export
setMethod(
  "+",
  signature(e1 = "ANY", e2 = "vctrs_vctr"),
  function(e1, e2) {
    vec_arith("+", e1, e2)
  }
)

#' @export
setMethod(
  "+",
  signature(e1 = "vctrs_vctr", e2 = "missing"),
  function(e1, e2) {
    vec_arith("+", e1, MISSING())
  }
)

#' @export
cbind.vctrs_vctr <- function(...) {
  vec_cbind(...)
}

#' @importFrom methods cbind2
#' @export
setMethod(
  "cbind2",
  signature(x = "vctrs_vctr", y = "ANY"),
  function(x, y, ...) {
    vec_cbind(x, y, ...)
  }
)

#' @export
setMethod(
  "cbind2",
  signature(x = "ANY", y = "vctrs_vctr"),
  function(x, y, ...) {
    vec_cbind(x, y, ...)
  }
)

#' @export
rbind.vctrs_vctr <- function(...) {
  vec_rbind(...)
}

#' @export
setMethod(
  "c",
  signature(x = "vctrs_vctr"),
  function(x, ...) {
    vec_c(x, ...)
  }
)

#' For S4 methods that require a documentation entry but only clutter the index.
#'
#' @usage NULL
#' @format NULL
#' @keywords internal
hidden_aliases <- NULL

#' @name hidden_aliases
#' @aliases
#'   +,ANY,vctrs_vctr-method
#'   +,vctrs_vctr,ANY-method
#'   +,vctrs_vctr,missing-method
#'   c,vctrs_vctr-method
#'   cbind2,ANY,vctrs_vctr-method
#'   cbind2,vctrs_vctr,ANY-method
#'   show,vctrs_vctr-method
NULL

validate_names <- function(.data) {
  nms <- names(.data)

  if (!names_all_or_nothing(nms)) {
    stop("If any elements of `.data` are named, all must be named", call. = FALSE)
  }

  nms
}
names_all_or_nothing <- function(names) {
  if (is.null(names)) {
    TRUE
  } else {
    all(names != "" & !is.na(names))
  }
}

#' @export
vec_proxy.vctrs_vctr <- function(x, ...) {
  if (is_list(x)) {
    unclass(x)
  } else {
    x
  }
}

#' @export
vec_restore.vctrs_vctr <- function(x, to, ..., i = NULL) {
  if (typeof(x) != typeof(to)) {
    stop_incompatible_cast(x, to)
  }
  NextMethod()
}

#' @method vec_cast vctrs_vctr
#' @export
vec_cast.vctrs_vctr <- function(x, to, ...) UseMethod("vec_cast.vctrs_vctr")

#' @method vec_cast.vctrs_vctr default
#' @export
vec_cast.vctrs_vctr.default <- function(x, to, ...) {
  # These are not strictly necessary, but make bootstrapping a new class
  # a bit simpler
  if (is.object(x)) {
    attr_x <- utils::modifyList(attributes(x), list(names = NULL))
    attr_y <- utils::modifyList(attributes(to), list(names = NULL))

    if (identical(attr_x, attr_y)) {
      return(x)
    } else {
      stop_incompatible_cast(x, to)
    }
  }

  vec_restore(x, to)
}

#' @export
c.vctrs_vctr <- function(...) {
  vec_c(...)
}

# Printing ----------------------------------------------------------------

#' @export
print.vctrs_vctr <- function(x, ...) {
  obj_print(x, ...)
  invisible(x)
}

#' @export
str.vctrs_vctr <- function(object, ...) {
  obj_str(object, ...)
}

#' @export
format.vctrs_vctr <- function(x, ...) {
  format(vec_data(x), ...)
}

# Subsetting --------------------------------------------------------------

#' @export
`[.vctrs_vctr` <- function(x, i, ...) {
  vec_index(x, i, ...)
}

#' @export
`[[.vctrs_vctr` <- function(x, i, ...) {
  if (is.list(x)) {
    NextMethod()
  } else {
    vec_restore(NextMethod(), x)
  }
}

#' @export
`$.vctrs_vctr` <- function(x, i) {
  if (is.list(x)) {
    NextMethod()
  } else {
    vec_restore(NextMethod(), x)
  }
}

#' @export
rep.vctrs_vctr <- function(x, ...) {
  vec_restore(NextMethod(), x)
}

#' @export
`length<-.vctrs_vctr` <- function(x, value) {
  vec_restore(NextMethod(), x)
}

#' @export
diff.vctrs_vctr <- function(x, lag = 1L, differences = 1L, ...) {
  stopifnot(length(lag) == 1L, lag >= 1L)
  stopifnot(length(differences) == 1L, differences >= 1L)

  n <- vec_size(x)
  if (lag * differences >= n)
    return(vec_slice(x, 0L))

  out <- x
  for (i in seq_len(differences)) {
    n <- vec_size(out)
    lhs <- (1L + lag):n
    rhs <- 1L:(n - lag)

    out <- vec_slice(out, lhs) - vec_slice(out, rhs)
  }

  out
}


# Modification -------------------------------------------------------------

#' @export
`[[<-.vctrs_vctr` <- function(x, ..., value) {
  if (!is.list(x)) {
    value <- vec_coercible_cast(value, x, x_arg = "x", to_arg = "value")
  }
  NextMethod()
}

#' @export
`$<-.vctrs_vctr` <- function(x, i, value) {
  if (is.list(x)) {
    NextMethod()
  } else {
    # Default behaviour is to cast LHS to a list
    abort("$ operator is invalid for atomic vectors.")
  }
}

#' @export
`[<-.vctrs_vctr` <- function(x, i, value) {
  value <- vec_coercible_cast(value, x, x_arg = "x", to_arg = "value")
  NextMethod()
}

#' @export
`names<-.vctrs_vctr` <- function(x, value) {
  if (length(value) != 0 && length(value) != length(x)) {
    abort("`names()` must be the same length as x.")
  }
  if (!names_all_or_nothing(value)) {
    abort("If any elements are named, all elements must be named.")
  }
  NextMethod()
}
# Coercion ----------------------------------------------------------------

#' @export
as.logical.vctrs_vctr <- function(x, ...) {
  vec_cast(x, logical())
}

#' @export
as.integer.vctrs_vctr <- function(x, ...) {
  vec_cast(x, integer())
}

#' @export
as.double.vctrs_vctr <- function(x, ...) {
  vec_cast(x, double())
}

#' @export
as.character.vctrs_vctr <- function(x, ...) {
  vec_cast(x, character())
}

#' @export
as.list.vctrs_vctr <- function(x, ...) {
  vec_cast(x, list())
}

#' @export
as.Date.vctrs_vctr <- function(x, ...) {
  vec_cast(x, date())
}

#' @export
as.POSIXct.vctrs_vctr <- function(x, tz = "", ...) {
  vec_cast(x, new_datetime(tzone = tz))
}

# Work around inconsistencies in as.data.frame() for 1D arrays
as.data.frame2 <- function(x) {
  out <- as.data.frame(x)

  if (vec_dim_n(x) == 1) {
    # 1D arrays are not stripped from their dimensions
    out[[1]] <- as.vector(out[[1]])

    # 1D arrays are auto-labelled with substitute()
    names(out) <- "V1"
  }

  out
}

#' @export
as.data.frame.vctrs_vctr <- function(x,
                                     row.names = NULL,
                                     optional = FALSE,
                                     ...,
                                     nm = paste(deparse(substitute(x), width.cutoff = 500L), collapse = " ")) {
  force(nm)

  if (has_dim(x)) {
    return(as.data.frame2(vec_data(x)))
  }

  cols <- list(x)
  if (!optional) {
    names(cols) <- nm
  }

  new_data_frame(cols, n = vec_size(x))
}

# Dynamically registered in .onLoad()
as.factor.vctrs_vctr <- function(x, levels = character(), ...) {
  vec_cast(x, new_factor(levels = levels))
}

# Dynamically registered in .onLoad()
as.ordered.vctrs_vctr <- function(x, levels = character(), ...) {
  vec_cast(x, new_ordered(levels = levels))
}

# Dynamically registered in .onLoad()
as.difftime.vctrs_vctr <- function(x, units = "secs", ...) {
  vec_cast(x, new_duration(units = units))
}

# Equality ----------------------------------------------------------------

#' @export
`==.vctrs_vctr` <- function(e1, e2) {
  vec_equal(e1, e2)
}

#' @export
`!=.vctrs_vctr` <- function(e1, e2) {
  !vec_equal(e1, e2)
}

#' @export
is.na.vctrs_vctr <- function(x) {
  vec_equal_na(x)
}

#' @export
anyNA.vctrs_vctr <- if (getRversion() >= "3.2") {
  function(x, recursive = FALSE) {
    any(is.na(x))
  }
} else {
  function(x) {
    any(is.na(x))
  }
}

#' @export
unique.vctrs_vctr <- function(x, incomparables = FALSE, ...) {
  vec_unique(x)
}

#' @export
duplicated.vctrs_vctr <- function(x, incomparables = FALSE, ...) {
  vec_duplicate_id(x) != seq_along(x)
}

#' @export
anyDuplicated.vctrs_vctr <- function(x, incomparables = FALSE, ...) {
  vec_duplicate_any(x)
}

# Comparison ----------------------------------------------------------------

#' @export
`<=.vctrs_vctr` <- function(e1, e2) {
  vec_compare(e1, e2) <= 0
}

#' @export
`<.vctrs_vctr` <- function(e1, e2) {
  vec_compare(e1, e2) < 0
}

#' @export
`>=.vctrs_vctr` <- function(e1, e2) {
  vec_compare(e1, e2) >= 0
}

#' @export
`>.vctrs_vctr` <- function(e1, e2) {
  vec_compare(e1, e2) > 0
}

#' @export
xtfrm.vctrs_vctr <- function(x) {
  proxy <- vec_proxy_compare(x)

  # order(order(x)) ~= rank(x)
  if (is_integer(proxy) || is_double(proxy)) {
    proxy
  } else {
    order(order_proxy(proxy))
  }
}

#' @importFrom stats median
#' @export
median.vctrs_vctr <- function(x, ..., na.rm = FALSE) {
  # nocov start
  stop_unimplemented(x, "median")
  # nocov end
}

#' @importFrom stats quantile
#' @export
quantile.vctrs_vctr <- function(x, ..., type = 1, na.rm = FALSE) {
  # nocov start
  stop_unimplemented(x, "quantile")
  # nocov end
}

#' @export
min.vctrs_vctr <- function(x, ..., na.rm = FALSE) {
  if (vec_is_empty(x)) {
    return(vec_cast(Inf, x))
  }

  # TODO: implement to do vec_arg_min()
  rank <- xtfrm(x)

  if (isTRUE(na.rm)) {
    idx <- which.min(rank)
  } else {
    idx <- which(vec_equal(rank, min(rank), na_equal = TRUE))
  }

  x[[idx[[1]]]]
}

#' @export
max.vctrs_vctr <- function(x, ..., na.rm = FALSE) {
  if (vec_is_empty(x)) {
    return(vec_cast(-Inf, x))
  }

  # TODO: implement to do vec_arg_max()
  rank <- xtfrm(x)

  if (isTRUE(na.rm)) {
    idx <- which.max(rank)
  } else {
    idx <- which(vec_equal(rank, max(rank), na_equal = TRUE))
  }

  x[[idx[[1]]]]
}

#' @export
range.vctrs_vctr <- function(x, ..., na.rm = FALSE) {
  if (vec_is_empty(x)) {
    return(vec_cast(c(Inf, -Inf), x))
  }

  # Inline `min()` / `max()` to only call `xtfrm()` once
  rank <- xtfrm(x)

  if (isTRUE(na.rm)) {
    idx_min <- which.min(rank)
    idx_max <- which.max(rank)
  } else {
    idx_min <- which(vec_equal(rank, min(rank), na_equal = TRUE))
    idx_max <- which(vec_equal(rank, max(rank), na_equal = TRUE))
  }

  c(x[[idx_min[[1]]]], x[[idx_max[[1]]]])
}

# Numeric -----------------------------------------------------------------

#' @export
Math.vctrs_vctr <- function(x, ...) {
  vec_math(.Generic, x, ...)
}

#' @export
Summary.vctrs_vctr <- function(..., na.rm = FALSE) {
  vec_math(.Generic, vec_c(...), na.rm = na.rm)
}

#' @export
mean.vctrs_vctr <- function(x, ..., na.rm = FALSE) {
  vec_math("mean", x, na.rm = na.rm)
}

#' @export
is.finite.vctrs_vctr <- function(x) {
  vec_math("is.finite", x)
}

#' @export
is.infinite.vctrs_vctr <- function(x) {
  vec_math("is.infinite", x)
}

#' @export
is.nan.vctrs_vctr <- function(x) {
  vec_math("is.nan", x)
}

# Arithmetic --------------------------------------------------------------



#' @export
`-.vctrs_vctr` <- function(e1, e2) {
  if (missing(e2)) {
    vec_arith("-", e1, MISSING())
  } else {
    vec_arith("-", e1, e2)
  }
}

#' @export
`*.vctrs_vctr` <- function(e1, e2) {
  vec_arith("*", e1, e2)
}

#' @export
`/.vctrs_vctr` <- function(e1, e2) {
  vec_arith("/", e1, e2)
}

#' @export
`^.vctrs_vctr` <- function(e1, e2) {
  vec_arith("^", e1, e2)
}

#' @export
`%%.vctrs_vctr` <- function(e1, e2) {
  vec_arith("%%", e1, e2)
}

#' @export
`%/%.vctrs_vctr` <- function(e1, e2) {
  vec_arith("%/%", e1, e2)
}

#' @export
`!.vctrs_vctr` <- function(x) {
  vec_arith("!", x, MISSING())
}

#' @export
`&.vctrs_vctr` <- function(e1, e2) {
  vec_arith("&", e1, e2)
}

#' @export
`|.vctrs_vctr` <- function(e1, e2) {
  vec_arith("|", e1, e2)
}

# Unimplemented ------------------------------------------------------------

#' @export
summary.vctrs_vctr <- function(object, ...) {
  # nocov start
  stop_unimplemented(object, "summary")
  # nocov end
}

# Unsupported --------------------------------------------------------------

#' @export
`dim<-.vctrs_vctr` <- function(x, value) {
  stop_unsupported(x, "dim<-")
}

#' @export
`dimnames<-.vctrs_vctr` <- function(x, value) {
  stop_unsupported(x, "dimnames<-")
}

#' @export
levels.vctrs_vctr <- function(x) {
  stop_unsupported(x, "levels")
}

#' @export
`levels<-.vctrs_vctr` <- function(x, value) {
  stop_unsupported(x, "levels<-")
}

#' @export
`t.vctrs_vctr` <- function(x) {
  stop_unsupported(x, "t")
}

#' @export
`is.na<-.vctrs_vctr` <- function(x, value) {
  # No support for other arguments than logical for now,
  # even if base R is more lenient here.
  vec_assert(value, logical())

  vec_slice(x, value) <- vec_init(x)
  x
}

# Helpers -----------------------------------------------------------------

# This simple class is used for testing as defining methods inside
# a test does not work (because the lexical scope is lost)
# nocov start
new_hidden <- function(x = double()) {
  stopifnot(is.numeric(x))
  new_vctr(vec_cast(x, double()), class = "hidden")
}
format.hidden <- function(x, ...) rep("xxx", length(x))

scoped_hidden <- function(frame = caller_env()) {
  scoped_bindings(.env = global_env(), .frame = frame,
    vec_ptype2.hidden         = function(x, y, ...) UseMethod("vec_ptype2.hidden", y),
    vec_ptype2.hidden.default = function(x, y, ...) stop_incompatible_type(x, y, ...),
    vec_ptype2.hidden.hidden  = function(x, y, ...) new_hidden(),
    vec_ptype2.hidden.double  = function(x, y, ...) new_hidden(),
    vec_ptype2.double.hidden  = function(x, y, ...) new_hidden(),
    vec_ptype2.hidden.logical = function(x, y, ...) new_hidden(),
    vec_ptype2.logical.hidden = function(x, y, ...) new_hidden(),

    vec_cast.hidden          = function(x, to, ...) UseMethod("vec_cast.hidden"),
    vec_cast.hidden.default  = function(x, to, ...) stop_incompatible_cast(x, to, ...),
    vec_cast.hidden.hidden   = function(x, to, ...) x,
    vec_cast.hidden.double   = function(x, to, ...) new_hidden(vec_data(x)),
    vec_cast.double.hidden   = function(x, to, ...) vec_data(x),
    vec_cast.hidden.logical  = function(x, to, ...) new_hidden(as.double(x)),
    vec_cast.logical.hidden  = function(x, to, ...) as.logical(vec_data(x))
  )
}

# nocov end
