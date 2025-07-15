#ifndef VCTRS_C_UNCHOP_H
#define VCTRS_C_UNCHOP_H

#include "vctrs-core.h"
#include "names.h"

r_obj* list_unchop(r_obj* xs,
                   r_obj* indices,
                   r_obj* default_,
                   r_obj* ptype,
                   r_obj* size,
                   r_obj* name_spec,
                   const struct name_repair_opts* name_repair,
                   struct vctrs_arg* p_error_arg,
                   struct vctrs_arg* p_default_arg,
                   struct r_lazy error_call);


#endif
