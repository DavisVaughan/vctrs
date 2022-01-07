// Initialised at load time
struct vctrs_arg args_start_;
static struct vctrs_arg* const args_start = &args_start_;

struct vctrs_arg args_end_;
static struct vctrs_arg* const args_end = &args_end_;

static r_obj* vec_interval_locate_minimal(r_obj* start,
                                          r_obj* end,
                                          bool keep_empty,
                                          bool keep_missing,
                                          bool groups);

static inline r_obj* interval_order(r_obj* compare, r_obj* start, r_obj* end);
