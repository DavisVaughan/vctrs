#include "order-groups.h"
#include "utils.h"

// -----------------------------------------------------------------------------

// Pair with `PROTECT_GROUP_INFO()` in the caller
struct group_info new_group_info() {
  return (struct group_info) {
    .data_size = 0,
    .data = R_NilValue,
    .n_groups = 0,
    .max_group_size = 0
  };
}

// -----------------------------------------------------------------------------

struct group_infos new_group_infos(struct group_info** p_p_group_info,
                                   R_xlen_t max_data_size,
                                   bool requested,
                                   bool ignore) {
  return (struct group_infos) {
    .p_p_group_info = p_p_group_info,
    .max_data_size = max_data_size,
    .current = 0,
    .requested = requested,
    .ignore = ignore
  };
}

// -----------------------------------------------------------------------------

static void group_realloc(struct group_info* p_group_info, R_xlen_t size);
static R_xlen_t groups_realloc_size(R_xlen_t data_size, R_xlen_t max_data_size);

/*
 * Push a group size onto the current `group_info*`
 * - Reallocates as needed
 * - Updates number of groups / max group size as well
 *
 * Should only be called through `groups_size_maybe_push()` to ensure
 * that we only push groups if we are tracking them.
 */
void groups_size_push(struct group_infos* p_group_infos, R_xlen_t size) {
  if (size == 0) {
    Rf_errorcall(R_NilValue, "Internal error: Group `size` to push should never be zero.");
  }

  struct group_info* p_group_info = groups_current(p_group_infos);

  // Extend `data` as required - reprotects itself
  if (p_group_info->data_size == p_group_info->n_groups) {
    R_xlen_t new_data_size = groups_realloc_size(
      p_group_info->data_size,
      p_group_infos->max_data_size
    );

    group_realloc(p_group_info, new_data_size);
  }

  // Push group size
  p_group_info->p_data[p_group_info->n_groups] = size;

  // Bump number of groups
  ++p_group_info->n_groups;

  // Update max group size
  if (p_group_info->max_group_size < size) {
    p_group_info->max_group_size = size;
  }
}

// -----------------------------------------------------------------------------

/*
 * Reallocate `data` to be as long as `size`.
 */
static
void group_realloc(struct group_info* p_group_info, R_xlen_t size) {
  // First allocation
  if (size == 0) {
    size = GROUP_DATA_SIZE_DEFAULT;
  }

  // Reallocate
  p_group_info->data = p_int_resize(
    p_group_info->p_data,
    p_group_info->data_size,
    size
  );

  // Reprotect
  REPROTECT(p_group_info->data, p_group_info->data_pi);

  // Update pointer
  p_group_info->p_data = INTEGER(p_group_info->data);

  // Update size
  p_group_info->data_size = size;
}

// -----------------------------------------------------------------------------

static
R_xlen_t groups_realloc_size(R_xlen_t data_size, R_xlen_t max_data_size) {
  // Avoid potential overflow when doubling size
  uint64_t new_data_size = ((uint64_t) data_size) * 2;

  // Clamp maximum allocation size to the size of the input
  if (new_data_size > max_data_size) {
    return max_data_size;
  }

  // Can now safely cast back to `R_xlen_t`
  return (R_xlen_t) new_data_size;
}

// -----------------------------------------------------------------------------

/*
 * `groups_swap()` is called after each data frame column is processed.
 * It handles switching the `current` group info that we are working on,
 * and ensures that the information that might have been there before has
 * been zeroed out. It also ensures that the new current group info has at
 * least as much space as the previous one, which is especially important for
 * the first column swap where the 2nd group info array starts as a size 0
 * integer vector (because we don't know if it will get used or not).
 */
void groups_swap(struct group_infos* p_group_infos) {
  if (p_group_infos->ignore) {
    return;
  }

  struct group_info* p_group_info_pre = groups_current(p_group_infos);

  // Make the swap
  p_group_infos->current = 1 - p_group_infos->current;

  struct group_info* p_group_info_post = groups_current(p_group_infos);

  // Clear the info from last time the swap was made
  p_group_info_post->max_group_size = 0;
  p_group_info_post->n_groups = 0;

  // Ensure the new group info is at least as big as the old group info
  if (p_group_info_post->data_size < p_group_info_pre->data_size) {
    R_xlen_t new_data_size = p_group_info_pre->data_size;
    group_realloc(p_group_info_post, new_data_size);
  }
}