
#include "ikarus.h"
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <math.h>


#if 0
ikp 
ikrt_is_flonum(ikp x){
  if(tagof(x) == vector_tag){
    if (ref(x, -vector_tag) == flonum_tag){
      return true_object;
    }
  }
  return false_object;
}
#endif

ikp
ikrt_string_to_flonum(ikp x, ikpcb* pcb){
  double v = strtod(string_data(x), NULL);
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
ikrt_fx_sin(ikp x, ikpcb* pcb){
  ikp r = ik_alloc(pcb, flonum_size) + vector_tag;
  ref(r, -vector_tag) = (ikp)flonum_tag;
  flonum_data(r) = sin(unfix(x));
  return r;
}







ikp
ikrt_flonum_to_string(ikp x, ikpcb* pcb){
  if(tagof(x) == vector_tag){
    if(ref(x,-vector_tag) == flonum_tag){
      char buff[80];
      int n = snprintf(buff, sizeof(buff)-2, "%.12G", flonum_data(x));
      if(n >= 0){
        int i=0;
        while ((i<n) && (buff[i] != '.')){ i++; }
        if(i == n){
          buff[i] = '.';
          buff[i+1] = '0';
          n += 2;;
        }
        ikp str = ik_alloc(pcb, align(n+disp_string_data+1)) +
          string_tag;
        ref(str, -string_tag) = fix(n);
        memcpy(string_data(str), buff, n);
        return str;
      }
    }
  }
  return false_object;
}

ikp
ikrt_fixnum_to_flonum(ikp x, ikpcb* pcb){
  ikp r = ik_alloc(pcb, flonum_size) + vector_tag;
  ref(r, -vector_tag) = (ikp)flonum_tag;
  flonum_data(r) = unfix(x);
  return r;
}

ikp
ikrt_bignum_to_flonum(ikp x, ikpcb* pcb){
  fprintf(stderr, "ERR in bignum_to_flonum\n");
  exit(-1);
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
