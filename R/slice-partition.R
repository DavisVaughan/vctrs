vec_partition <- function(x, sizes) {
  .Call(ffi_vec_partition, x, sizes)
}
