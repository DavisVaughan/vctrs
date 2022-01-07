// Initialised at load time
struct vctrs_arg args_start_;
static struct vctrs_arg* const args_start = &args_start_;

struct vctrs_arg args_end_;
static struct vctrs_arg* const args_end = &args_end_;

struct vctrs_arg args_lower_;
static struct vctrs_arg* const args_lower = &args_lower_;

struct vctrs_arg args_upper_;
static struct vctrs_arg* const args_upper = &args_upper_;

static r_obj* vec_interval_locate_minimal(r_obj* start,
                                          r_obj* end,
                                          bool keep_empty,
                                          bool keep_missing,
                                          bool groups);

static r_obj* vec_interval_complement(r_obj* start,
                                      r_obj* end,
                                      r_obj* lower,
                                      r_obj* upper);

static inline r_obj* interval_order(r_obj* compare, r_obj* start, r_obj* end);
static inline r_obj* interval_detect_complete(r_obj* start, r_obj* end);
