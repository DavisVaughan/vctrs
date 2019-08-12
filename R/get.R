vec_is_recursive <- function(x) {
  is.list(x) && !is.data.frame(x) && vec_is_vector(x)
}

vec_get <- function(x, i) {
  if (is.null(x)) {
    return(x)
  }

  vec_assert(x)

  if (vec_size(i) != 1L) {
    stop("size of `i` must be 1.")
  }

  # can't get out of bounds
  i <- vec_as_index(i, vec_size(x), vec_names(x))

  # For lists, return the inner element
  if (vec_is_recursive(x)) {
    slice <- x[[i]]
    return(slice)
  }

  slice <- vec_slice(x, i)

  # Drop vec_names here?

  slice
}

vec_set <- function(x, i, value) {
  if (is.null(x)) {
    return(x)
  }

  vec_assert(x)

  if (vec_size(i) != 1L) {
    stop("size of `i` must be 1.")
  }

  # can't set out of bounds
  i <- vec_as_index(i, vec_size(x), vec_names(x))

  if (vec_is_recursive(x)) {
    if (is.null(value)) {
      x[i] <- list(value)
    } else {
      x[[i]] <- value
    }

    return(x)
  }

  # with a coercible cast
  x <- vec_assign(x, i, value)

  x
}
