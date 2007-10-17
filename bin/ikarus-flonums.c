
#include "ikarus-data.h"
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <math.h>

ikp
ikrt_fl_round(ikp x, ikp y){
  flonum_data(y) = round(flonum_data(x));
  return y;
}

ikp
ikrt_fl_exp(ikp x, ikp y){
  flonum_data(y) = exp(flonum_data(x));
  return y;
}

ikp
ikrt_flfl_expt(ikp a, ikp b, ikp z){
  flonum_data(z) = exp(flonum_data(b) * log(flonum_data(a)));
  return z;
}





ikp
ikrt_bytevector_to_flonum(ikp x, ikpcb* pcb){
  double v = strtod((char*)x+off_bytevector_data, NULL);
  ikp r = ik_alloc(pcb, flonum_size) + vector_tag;
  ref(r, -vector_tag) = (ikp)flonum_tag;
  flonum_data(r) = v;
  return r;
}


ikp
ikrt_fl_plus(ikp x, ikp y,ikpcb* pcb){
  ikp r = ik_alloc(pcb, flonum_size) + vector_tag;
  ref(r, -vector_tag) = (ikp)flonum_tag;
  flonum_data(r) = flonum_data(x) + flonum_data(y);
  return r;
}

ikp
ikrt_fl_minus(ikp x, ikp y,ikpcb* pcb){
  ikp r = ik_alloc(pcb, flonum_size) + vector_tag;
  ref(r, -vector_tag) = (ikp)flonum_tag;
  flonum_data(r) = flonum_data(x) - flonum_data(y);
  return r;
}

ikp
ikrt_fl_times(ikp x, ikp y,ikpcb* pcb){
  ikp r = ik_alloc(pcb, flonum_size) + vector_tag;
  ref(r, -vector_tag) = (ikp)flonum_tag;
  flonum_data(r) = flonum_data(x) * flonum_data(y);
  return r;
}

ikp
ikrt_fl_div(ikp x, ikp y,ikpcb* pcb){
  ikp r = ik_alloc(pcb, flonum_size) + vector_tag;
  ref(r, -vector_tag) = (ikp)flonum_tag;
  flonum_data(r) = flonum_data(x) / flonum_data(y);
  return r;
}

ikp
ikrt_fl_invert(ikp x, ikpcb* pcb){
  ikp r = ik_alloc(pcb, flonum_size) + vector_tag;
  ref(r, -vector_tag) = (ikp)flonum_tag;
  flonum_data(r) = 1.0 / flonum_data(x);
  return r;
}

ikp
ikrt_fl_sin(ikp x, ikpcb* pcb){
  ikp r = ik_alloc(pcb, flonum_size) + vector_tag;
  ref(r, -vector_tag) = (ikp)flonum_tag;
  flonum_data(r) = sin(flonum_data(x));
  return r;
}

ikp
ikrt_fl_cos(ikp x, ikpcb* pcb){
  ikp r = ik_alloc(pcb, flonum_size) + vector_tag;
  ref(r, -vector_tag) = (ikp)flonum_tag;
  flonum_data(r) = cos(flonum_data(x));
  return r;
}

ikp
ikrt_fl_tan(ikp x, ikpcb* pcb){
  ikp r = ik_alloc(pcb, flonum_size) + vector_tag;
  ref(r, -vector_tag) = (ikp)flonum_tag;
  flonum_data(r) = tan(flonum_data(x));
  return r;
}

ikp
ikrt_fl_asin(ikp x, ikpcb* pcb){
  ikp r = ik_alloc(pcb, flonum_size) + vector_tag;
  ref(r, -vector_tag) = (ikp)flonum_tag;
  flonum_data(r) = asin(flonum_data(x));
  return r;
}

ikp
ikrt_fl_acos(ikp x, ikpcb* pcb){
  ikp r = ik_alloc(pcb, flonum_size) + vector_tag;
  ref(r, -vector_tag) = (ikp)flonum_tag;
  flonum_data(r) = acos(flonum_data(x));
  return r;
}

ikp
ikrt_fl_atan(ikp x, ikpcb* pcb){
  ikp r = ik_alloc(pcb, flonum_size) + vector_tag;
  ref(r, -vector_tag) = (ikp)flonum_tag;
  flonum_data(r) = atan(flonum_data(x));
  return r;
}




ikp
ikrt_fl_sqrt(ikp x, ikpcb* pcb){
  ikp r = ik_alloc(pcb, flonum_size) + vector_tag;
  ref(r, -vector_tag) = (ikp)flonum_tag;
  flonum_data(r) = sqrt(flonum_data(x));
  return r;
}

ikp
ikrt_fl_log(ikp x, ikpcb* pcb){
  ikp r = ik_alloc(pcb, flonum_size) + vector_tag;
  ref(r, -vector_tag) = (ikp)flonum_tag;
  flonum_data(r) = log(flonum_data(x));
  return r;
}


ikp
ikrt_fx_sin(ikp x, ikpcb* pcb){
  ikp r = ik_alloc(pcb, flonum_size) + vector_tag;
  ref(r, -vector_tag) = (ikp)flonum_tag;
  flonum_data(r) = sin(unfix(x));
  return r;
}

ikp
ikrt_fx_cos(ikp x, ikpcb* pcb){
  ikp r = ik_alloc(pcb, flonum_size) + vector_tag;
  ref(r, -vector_tag) = (ikp)flonum_tag;
  flonum_data(r) = cos(unfix(x));
  return r;
}

ikp
ikrt_fx_tan(ikp x, ikpcb* pcb){
  ikp r = ik_alloc(pcb, flonum_size) + vector_tag;
  ref(r, -vector_tag) = (ikp)flonum_tag;
  flonum_data(r) = tan(unfix(x));
  return r;
}

ikp
ikrt_fx_asin(ikp x, ikpcb* pcb){
  ikp r = ik_alloc(pcb, flonum_size) + vector_tag;
  ref(r, -vector_tag) = (ikp)flonum_tag;
  flonum_data(r) = asin(unfix(x));
  return r;
}

ikp
ikrt_fx_acos(ikp x, ikpcb* pcb){
  ikp r = ik_alloc(pcb, flonum_size) + vector_tag;
  ref(r, -vector_tag) = (ikp)flonum_tag;
  flonum_data(r) = acos(unfix(x));
  return r;
}

ikp
ikrt_fx_atan(ikp x, ikpcb* pcb){
  ikp r = ik_alloc(pcb, flonum_size) + vector_tag;
  ref(r, -vector_tag) = (ikp)flonum_tag;
  flonum_data(r) = atan(unfix(x));
  return r;
}

ikp
ikrt_fx_sqrt(ikp x, ikpcb* pcb){
  ikp r = ik_alloc(pcb, flonum_size) + vector_tag;
  ref(r, -vector_tag) = (ikp)flonum_tag;
  flonum_data(r) = sqrt(unfix(x));
  return r;
}


ikp
ikrt_fx_log(ikp x, ikpcb* pcb){
  ikp r = ik_alloc(pcb, flonum_size) + vector_tag;
  ref(r, -vector_tag) = (ikp)flonum_tag;
  flonum_data(r) = log(unfix(x));
  return r;
}


ikp
ikrt_fixnum_to_flonum(ikp x, ikpcb* pcb){
  ikp r = ik_alloc(pcb, flonum_size) + vector_tag;
  ref(r, -vector_tag) = (ikp)flonum_tag;
  flonum_data(r) = unfix(x);
  return r;
}

ikp
ikrt_fl_equal(ikp x, ikp y){
  if(flonum_data(x) == flonum_data(y)){
    return true_object;
  } else {
    return false_object;
  }
}

ikp
ikrt_fl_less_or_equal(ikp x, ikp y){
  if(flonum_data(x) <= flonum_data(y)){
    return true_object;
  } else {
    return false_object;
  }
}

ikp
ikrt_fl_less(ikp x, ikp y){
  if(flonum_data(x) < flonum_data(y)){
    return true_object;
  } else {
    return false_object;
  }
}
