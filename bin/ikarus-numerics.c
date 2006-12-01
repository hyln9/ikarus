
#include "ikarus.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <gmp.h>


#define most_positive_fixnum 0x1FFFFFFF
#define most_negative_fixnum 0x20000000

#define max_digits_per_limb 10

ikp 
ikrt_isbignum(ikp x){
  if(tagof(x) == vector_tag){
    ikp fst = ref(x, -vector_tag);
    if (bignum_tag == (bignum_mask & (int)fst)){
      return true_object;
    }
  }
  return false_object;
}

ikp
ikrt_positive_bn(ikp x){
  ikp fst = ref(x, -vector_tag);
  if((int)fst & bignum_sign_mask){
    return false_object;
  } else {
    return true_object;
  }
}


ikp 
ikrt_fxfxplus(ikp x, ikp y, ikpcb* pcb){
  int n1 = unfix(x);
  int n2 = unfix(y);
  int r = n1 + n2;
  ikp q = fix(r);
  if(r == unfix(q)){
    return q;
  } 
  else {
    ikp bn = ik_alloc(pcb, align(disp_bignum_data + wordsize));
    if(r > 0){
      ref(bn, 0) = (ikp)(bignum_tag | (1 << bignum_length_shift)); 
      ref(bn, disp_bignum_data) = (ikp)r;
    }
    else {
      ref(bn, 0) = 
        (ikp)(bignum_tag | 
              (1 << bignum_length_shift) | 
              (1 << bignum_sign_shift));
      ref(bn, disp_bignum_data) = (ikp)-r;
    }
    return bn+vector_tag;
  }
}

ikp 
ikrt_fxbnplus(ikp x, ikp y, ikpcb* pcb){
  if(x == 0){ return y ; }
  ikp fst = ref(y, -vector_tag);
  int limb_count = ((unsigned int) fst) >> bignum_length_shift;
  int intx = unfix(x);
  if(intx > 0){
    if((bignum_sign_mask & (int)fst) == 0){
      /* positive fx + positive bn = even bigger positive */
      ikp r = ik_alloc(pcb, align(disp_bignum_data+(limb_count+1)*wordsize));
      int carry = mpn_add_1((mp_limb_t*)(r+disp_bignum_data), 
                            (mp_limb_t*)(y - vector_tag + disp_bignum_data),
                            limb_count,
                            intx);
      if(carry){
        ref(r, disp_bignum_data + limb_count*wordsize) = (ikp)1;
        ref(r, 0) = (ikp)
             (((limb_count + 1) << bignum_length_shift) |
              (0 << bignum_sign_shift) |
              bignum_tag);
        return r+vector_tag;
      } else {
        ref(r, 0) = (ikp)
          ((limb_count << bignum_length_shift) |
           (0 << bignum_sign_shift) |
           bignum_tag);
        return r+vector_tag;
      }
    }
    else {
      /* positive fx + negative bn = smaller negative bn */
      ikp r = ik_alloc(pcb, align(disp_bignum_data+limb_count*wordsize));
      int borrow = mpn_sub_1((mp_limb_t*)(r+disp_bignum_data), 
                             (mp_limb_t*)(y - vector_tag + disp_bignum_data),
                             limb_count,
                             intx);
      if(borrow){
        fprintf(stderr, "Error: BUG in borrow\n");
        exit(-1);
      }
      int result_size = 
        (ref(r, disp_bignum_data + (limb_count-1)*wordsize)) 
        ? limb_count
        : (limb_count - 1);
      if(result_size == 0){ 
        return 0;
      }
      if(result_size == 1){
        unsigned int last = 
          (unsigned int) ref(r, disp_bignum_data + (result_size-1)*wordsize);
        if(last <= most_negative_fixnum){
          return fix(-(int)last);
        }
      }
      ref(r, 0) = (ikp)
        ((result_size << bignum_length_shift) |
         (1 << bignum_sign_shift) |
         bignum_tag);
      return r+vector_tag;
    }
  }
  else {
    if((bignum_sign_mask & (int)fst) == 0){
      /* negative fx + positive bn = smaller positive */
      ikp r = ik_alloc(pcb, align(disp_bignum_data+limb_count*wordsize));
      int borrow = mpn_sub_1((mp_limb_t*)(r+disp_bignum_data), 
                             (mp_limb_t*)(y - vector_tag + disp_bignum_data),
                             limb_count,
                             - intx);
      if(borrow){
        fprintf(stderr, "Error: BUG in borrow\n");
        exit(-1);
      }
      int result_size = 
        (ref(r, disp_bignum_data + (limb_count-1)*wordsize) == 0) 
        ? (limb_count - 1)
        : limb_count;
      if(result_size == 0){
        return 0;
      }
      if(result_size == 1){
        unsigned int last = 
          (unsigned int) ref(r, disp_bignum_data + (result_size-1)*wordsize);
        if(last <= most_positive_fixnum){
          return fix((int)last);
        }
      }
      ref(r, 0) = (ikp)
        ((result_size << bignum_length_shift) |
         (0 << bignum_sign_shift) |
         bignum_tag);
      return r+vector_tag;
    } else {
      /* negative fx + negative bn = larger negative */
      ikp r = ik_alloc(pcb, align(disp_bignum_data+(limb_count+1)*wordsize));
      int carry = mpn_add_1((mp_limb_t*)(r+disp_bignum_data), 
                            (mp_limb_t*)(y - vector_tag + disp_bignum_data),
                            limb_count,
                            -intx);
      if(carry){
        ref(r, disp_bignum_data + limb_count*wordsize) = (ikp)1;
        ref(r, 0) = (ikp)
             (((limb_count + 1) << bignum_length_shift) |
              (1 << bignum_sign_shift) |
              bignum_tag);
        return r+vector_tag;
      } else {
        ref(r, 0) = (ikp)
          ((limb_count << bignum_length_shift) |
           (1 << bignum_sign_shift) |
           bignum_tag);
        return r+vector_tag;
      }
    }
  }
}




ikp 
ikrt_bnbnplus(ikp x, ikp y, ikpcb* pcb){
  unsigned int xfst = (unsigned int)ref(x, -vector_tag);
  unsigned int yfst = (unsigned int)ref(y, -vector_tag);
  int xsign = xfst & bignum_sign_mask;
  int ysign = yfst & bignum_sign_mask;
  int xlimbs = xfst >> bignum_length_shift;
  int ylimbs = yfst >> bignum_length_shift;
  if(xsign == ysign){
    int n1,n2;
    ikp s1,s2;
    if(xlimbs > ylimbs){
      n1 = xlimbs; n2 = ylimbs; s1 = x; s2 = y;
    } else {
      n1 = ylimbs; n2 = xlimbs; s1 = y; s2 = x;
    }
    ikp res = ik_alloc(pcb, align(disp_bignum_data + (n1+1)*wordsize));
    mp_limb_t carry = mpn_add((mp_limb_t*) (res+disp_bignum_data),
                              (mp_limb_t*) (s1-vector_tag+disp_bignum_data),
                              n1,
                              (mp_limb_t*) (s2-vector_tag+disp_bignum_data),
                              n2);
    if(carry){
      ref(res, disp_vector_data + xlimbs*wordsize) = (ikp)1;
      ref(res, 0) = (ikp)
                    (((n1+1) << bignum_length_shift) |
                     xsign |
                     bignum_tag);
      return res+vector_tag;
    } else {
      ref(res, 0) = (ikp)
                    ((n1 << bignum_length_shift) |
                     xsign |
                     bignum_tag);
      return res+vector_tag;
    }
  }
  else {
    ikp s1=x, s2=y;
    int n1=xlimbs, n2=ylimbs;
    int result_sign = xsign;
    while((xlimbs == ylimbs) && 
          (ref(x, -vector_tag+disp_bignum_data+(xlimbs-1)*wordsize) == 
           ref(y, -vector_tag+disp_bignum_data+(xlimbs-1)*wordsize))){
      xlimbs -= 1;
      ylimbs -= 1;
      if(xlimbs == 0){ return 0; }
    }
    /* |x| != |y| */
    if(xlimbs <= ylimbs){
      if(xlimbs == ylimbs){
        if((ref(y, -vector_tag+disp_bignum_data+(xlimbs-1)*wordsize) > 
            ref(x, -vector_tag+disp_bignum_data+(xlimbs-1)*wordsize))){
          s1 = y; n1 = ylimbs;
          s2 = x; n2 = xlimbs;
          result_sign = ysign;
        }
      } else {
        s1 = y; n1 = ylimbs;
        s2 = x; n2 = xlimbs;
        result_sign = ysign;
      }
    }
    /* |s1| > |s2| */
    ikp res = ik_alloc(pcb, align(disp_bignum_data + n1 * wordsize));
    int burrow = mpn_sub((mp_limb_t*) (res + disp_bignum_data),
                         (mp_limb_t*) (s1 - vector_tag + disp_bignum_data),
                         n1,
                         (mp_limb_t*) (s2 - vector_tag + disp_bignum_data),
                         n2);
    if(burrow){
      fprintf(stderr, "BUG: Burrow error in bnbn+\n");
      exit(-1);
    }
    int len = n1;
    while(ref(res, disp_bignum_data + (len-1)*wordsize) == 0){
      len--;
      if(len == 0){
        return 0;
      }
    }
    if(result_sign == 0){
      /* positive result */
      if(len == 1){
        unsigned int fst_limb = (unsigned int) ref(res, disp_bignum_data);
        if(fst_limb <= most_positive_fixnum){
          return fix((int)fst_limb);
        }
      }
      ref(res, 0) = (ikp)
                    ((len << bignum_length_shift) |
                     result_sign |
                     bignum_tag);
      return res+vector_tag;
    } else {
      /* negative result */
      if(len == 1){
        unsigned int fst_limb = (unsigned int) ref(res, disp_bignum_data);
        if(fst_limb <= most_negative_fixnum){
          return fix(-(int)fst_limb);
        }
      }
      ref(res, 0) = (ikp)
                    ((len << bignum_length_shift) |
                     result_sign |
                     bignum_tag);
      return res+vector_tag;
    }
  }
}




ikp 
ikrt_fxfxminus(ikp x, ikp y, ikpcb* pcb){
  int n1 = unfix(x);
  int n2 = unfix(y);
  int r = n1 - n2;
  if(r >= 0){
    if(((unsigned int)r) <= most_positive_fixnum){
      return fix(r);
    } else {
      ikp bn = ik_alloc(pcb, align(disp_bignum_data + wordsize));
      ref(bn, 0) = (ikp) (bignum_tag | (1 << bignum_length_shift));
      ref(bn, disp_bignum_data) = (ikp)r;
      return bn+vector_tag;
    }
  } else {
    if(((unsigned int)r) <= most_negative_fixnum){
      return fix(r);
    } else {
      ikp bn = ik_alloc(pcb, align(disp_bignum_data + wordsize));
      ref(bn, 0) = (ikp) 
        (bignum_tag | 
         (1 << bignum_sign_shift) |
         (1 << bignum_length_shift));
      ref(bn, disp_bignum_data) = (ikp)(-r);
      return bn+vector_tag;
    }
  }
}


ikp 
ikrt_bnnegate(ikp x, ikpcb* pcb){
  ikp fst = ref(x, -vector_tag);
  int limb_count = ((unsigned int) fst) >> bignum_length_shift;
  if(limb_count == 1){
    if((bignum_sign_mask & (int)fst) == 0){
      /* positive bignum */
      unsigned int limb = (unsigned int) ref(x, disp_bignum_data - vector_tag);
      if(limb == (most_positive_fixnum + 1)){
        return fix(-(int)limb);
      }
    }
  }
  ikp bn = ik_alloc(pcb, align(disp_bignum_data + limb_count * wordsize));
  memcpy(bn+disp_bignum_data,
         x-vector_tag+disp_bignum_data,
         limb_count*wordsize);
  ref(bn, 0) = (ikp)
    (bignum_tag |
     ((1 << bignum_sign_shift) - (bignum_sign_mask & (int)fst)) |
     (limb_count << bignum_length_shift));
  return bn+vector_tag;
}

ikp 
ikrt_fxbnminus(ikp x, ikp y, ikpcb* pcb){
  if(x == 0){ return ikrt_bnnegate(y, pcb) ; }
  ikp fst = ref(y, -vector_tag);
  int limb_count = ((unsigned int) fst) >> bignum_length_shift;
  int intx = unfix(x);
  if(intx > 0){
    if(bignum_sign_mask & (int)fst){
      /* positive fx - negative bn = positive bn */
      ikp r = ik_alloc(pcb, align(disp_bignum_data+(limb_count+1)*wordsize));
      int carry = mpn_add_1((mp_limb_t*)(r+disp_bignum_data), 
                            (mp_limb_t*)(y - vector_tag + disp_bignum_data),
                            limb_count,
                            intx);
      if(carry){
        ref(r, disp_bignum_data + limb_count*wordsize) = (ikp)1;
        ref(r, 0) = (ikp)
             (((limb_count + 1) << bignum_length_shift) |
              (0 << bignum_sign_shift) |
              bignum_tag);
        return r+vector_tag;
      } else {
        ref(r, 0) = (ikp)
          ((limb_count << bignum_length_shift) |
           (0 << bignum_sign_shift) |
           bignum_tag);
        return r+vector_tag;
      }
    }
    else {
      /* positive fx - positive bn = smaller negative bn/fx */
      ikp r = ik_alloc(pcb, align(disp_bignum_data+limb_count*wordsize));
      int borrow = mpn_sub_1((mp_limb_t*)(r+disp_bignum_data), 
                             (mp_limb_t*)(y - vector_tag + disp_bignum_data),
                             limb_count,
                             intx);
      if(borrow){
        fprintf(stderr, "Error: BUG in borrow\n");
        exit(-1);
      }
      int result_size = 
        (ref(r, disp_bignum_data + (limb_count-1)*wordsize)) 
        ? limb_count
        : (limb_count - 1);
      if(result_size == 0){ 
        return 0;
      }
      if(result_size == 1){
        unsigned int last = 
          (unsigned int) ref(r, disp_bignum_data + (result_size-1)*wordsize);
        if(last <= most_negative_fixnum){
          return fix(-(int)last);
        }
      }
      ref(r, 0) = (ikp)
        ((result_size << bignum_length_shift) |
         (1 << bignum_sign_shift) |
         bignum_tag);
      return r+vector_tag;
    }
  }
  else {
    if(bignum_sign_mask & (int)fst){
      /* negative fx - negative bn = smaller positive */
      ikp r = ik_alloc(pcb, align(disp_bignum_data+limb_count*wordsize));
      int borrow = mpn_sub_1((mp_limb_t*)(r+disp_bignum_data), 
                             (mp_limb_t*)(y - vector_tag + disp_bignum_data),
                             limb_count,
                             - intx);
      if(borrow){
        fprintf(stderr, "Error: BUG in borrow\n");
        exit(-1);
      }
      int result_size = 
        (ref(r, disp_bignum_data + (limb_count-1)*wordsize) == 0) 
        ? (limb_count - 1)
        : limb_count;
      if(result_size == 0){
        return 0;
      }
      if(result_size == 1){
        unsigned int last = 
          (unsigned int) ref(r, disp_bignum_data + (result_size-1)*wordsize);
        if(last <= most_positive_fixnum){
          return fix((int)last);
        }
      }
      ref(r, 0) = (ikp)
        ((result_size << bignum_length_shift) |
         (0 << bignum_sign_shift) |
         bignum_tag);
      return r+vector_tag;
    } else {
      /* negative fx - positive bn = larger negative */
      ikp r = ik_alloc(pcb, align(disp_bignum_data+(limb_count+1)*wordsize));
      int carry = mpn_add_1((mp_limb_t*)(r+disp_bignum_data), 
                            (mp_limb_t*)(y - vector_tag + disp_bignum_data),
                            limb_count,
                            -intx);
      if(carry){
        ref(r, disp_bignum_data + limb_count*wordsize) = (ikp)1;
        ref(r, 0) = (ikp)
             (((limb_count + 1) << bignum_length_shift) |
              (1 << bignum_sign_shift) |
              bignum_tag);
        return r+vector_tag;
      } else {
        ref(r, 0) = (ikp)
          ((limb_count << bignum_length_shift) |
           (1 << bignum_sign_shift) |
           bignum_tag);
        return r+vector_tag;
      }
    }
  }
}

ikp 
ikrt_bnfxminus(ikp x, ikp y, ikpcb* pcb){
  if(y == 0){ return x; }
  ikp fst = ref(x, -vector_tag);
  int limb_count = ((unsigned int) fst) >> bignum_length_shift;
  int inty = unfix(y);
  if(inty < 0){
    if((bignum_sign_mask & (int)fst) == 0){
      /* - negative fx + positive bn = positive bn */
      ikp r = ik_alloc(pcb, align(disp_bignum_data+(limb_count+1)*wordsize));
      int carry = mpn_add_1((mp_limb_t*)(r+disp_bignum_data), 
                            (mp_limb_t*)(x - vector_tag + disp_bignum_data),
                            limb_count,
                            -inty);
      if(carry){
        ref(r, disp_bignum_data + limb_count*wordsize) = (ikp)1;
        ref(r, 0) = (ikp)
             (((limb_count + 1) << bignum_length_shift) |
              (0 << bignum_sign_shift) |
              bignum_tag);
        return r+vector_tag;
      } else {
        ref(r, 0) = (ikp)
          ((limb_count << bignum_length_shift) |
           (0 << bignum_sign_shift) |
           bignum_tag);
        return r+vector_tag;
      }
    }
    else {
      /* - negative fx + negative bn = smaller negative bn/fx */
      ikp r = ik_alloc(pcb, align(disp_bignum_data+limb_count*wordsize));
      int borrow = mpn_sub_1((mp_limb_t*)(r+disp_bignum_data), 
                             (mp_limb_t*)(x - vector_tag + disp_bignum_data),
                             limb_count,
                             -inty);
      if(borrow){
        fprintf(stderr, "Error: BUG in borrow\n");
        exit(-1);
      }
      int result_size = 
        (ref(r, disp_bignum_data + (limb_count-1)*wordsize)) 
        ? limb_count
        : (limb_count - 1);
      if(result_size == 0){ 
        return 0;
      }
      if(result_size == 1){
        unsigned int last = 
          (unsigned int) ref(r, disp_bignum_data + (result_size-1)*wordsize);
        if(last <= most_negative_fixnum){
          return fix(-(int)last);
        }
      }
      ref(r, 0) = (ikp)
        ((result_size << bignum_length_shift) |
         (1 << bignum_sign_shift) |
         bignum_tag);
      return r+vector_tag;
    }
  }
  else {
    if((bignum_sign_mask & (int)fst) == 0){
      /* - positive fx + positive bn = smaller positive */
      ikp r = ik_alloc(pcb, align(disp_bignum_data+limb_count*wordsize));
      int borrow = mpn_sub_1((mp_limb_t*)(r+disp_bignum_data), 
                             (mp_limb_t*)(x - vector_tag + disp_bignum_data),
                             limb_count,
                             inty);
      if(borrow){
        fprintf(stderr, "Error: BUG in borrow\n");
        exit(-1);
      }
      int result_size = 
        (ref(r, disp_bignum_data + (limb_count-1)*wordsize) == 0) 
        ? (limb_count - 1)
        : limb_count;
      if(result_size == 0){
        return 0;
      }
      if(result_size == 1){
        unsigned int last = 
          (unsigned int) ref(r, disp_bignum_data + (result_size-1)*wordsize);
        if(last <= most_positive_fixnum){
          return fix((int)last);
        }
      }
      ref(r, 0) = (ikp)
        ((result_size << bignum_length_shift) |
         (0 << bignum_sign_shift) |
         bignum_tag);
      return r+vector_tag;
    } else {
      /* - positive fx + negative bn = larger negative */
      ikp r = ik_alloc(pcb, align(disp_bignum_data+(limb_count+1)*wordsize));
      int carry = mpn_add_1((mp_limb_t*)(r+disp_bignum_data), 
                            (mp_limb_t*)(x - vector_tag + disp_bignum_data),
                            limb_count,
                            inty);
      if(carry){
        ref(r, disp_bignum_data + limb_count*wordsize) = (ikp)1;
        ref(r, 0) = (ikp)
             (((limb_count + 1) << bignum_length_shift) |
              (1 << bignum_sign_shift) |
              bignum_tag);
        return r+vector_tag;
      } else {
        ref(r, 0) = (ikp)
          ((limb_count << bignum_length_shift) |
           (1 << bignum_sign_shift) |
           bignum_tag);
        return r+vector_tag;
      }
    }
  }
}



ikp 
ikrt_bnbnminus(ikp x, ikp y, ikpcb* pcb){
  if(x == y) { return 0; }
  unsigned int xfst = (unsigned int)ref(x, -vector_tag);
  unsigned int yfst = (unsigned int)ref(y, -vector_tag);
  int xsign = xfst & bignum_sign_mask;
  int ysign = yfst & bignum_sign_mask;
  int xlimbs = xfst >> bignum_length_shift;
  int ylimbs = yfst >> bignum_length_shift;
  if(xsign != ysign){
    int n1,n2;
    ikp s1,s2;
    if(xlimbs >= ylimbs){
      n1 = xlimbs; n2 = ylimbs; s1 = x; s2 = y;
    } else {
      n1 = ylimbs; n2 = xlimbs; s1 = y; s2 = x;
    }
    ikp res = ik_alloc(pcb, align(disp_bignum_data + (n1+1)*wordsize));
    mp_limb_t carry = mpn_add((mp_limb_t*) (res+disp_bignum_data),
                              (mp_limb_t*) (s1-vector_tag+disp_bignum_data),
                              n1,
                              (mp_limb_t*) (s2-vector_tag+disp_bignum_data),
                              n2);
    if(carry){
      ref(res, disp_vector_data + xlimbs*wordsize) = (ikp)1;
      ref(res, 0) = (ikp)
                    (((n1+1) << bignum_length_shift) |
                     xsign |
                     bignum_tag);
      return res+vector_tag;
    } else {
      ref(res, 0) = (ikp)
                    ((n1 << bignum_length_shift) |
                     xsign |
                     bignum_tag);
      return res+vector_tag;
    }
  }
  else {
    /* same sign */
    if(xlimbs == ylimbs){
      while((ref(x, -vector_tag+disp_bignum_data+(xlimbs-1)*wordsize) == 
             ref(y, -vector_tag+disp_bignum_data+(xlimbs-1)*wordsize))){
        xlimbs -= 1;
        if(xlimbs == 0){ return 0; }
      }
      ylimbs = xlimbs;
    }
    ikp s1=x, s2=y;
    int n1=xlimbs, n2=ylimbs;
    int result_sign = xsign;
    /* |x| != |y| */
    if(xlimbs <= ylimbs){
      if(xlimbs == ylimbs){
        if((ref(y, -vector_tag+disp_bignum_data+(xlimbs-1)*wordsize) > 
            ref(x, -vector_tag+disp_bignum_data+(xlimbs-1)*wordsize))){
          s1 = y; n1 = ylimbs;
          s2 = x; n2 = xlimbs;
          result_sign = (1 << bignum_length_shift) - ysign;
        }
      } else {
        s1 = y; n1 = ylimbs;
        s2 = x; n2 = xlimbs;
        result_sign = (1 << bignum_length_shift) - ysign;
      }
    }
    /* |s1| > |s2| */
    ikp res = ik_alloc(pcb, align(disp_bignum_data + n1 * wordsize));
    int burrow = mpn_sub((mp_limb_t*) (res + disp_bignum_data),
                         (mp_limb_t*) (s1 - vector_tag + disp_bignum_data),
                         n1,
                         (mp_limb_t*) (s2 - vector_tag + disp_bignum_data),
                         n2);
    if(burrow){
      fprintf(stderr, "BUG: Burrow error in bnbn-\n");
      exit(-1);
    }
    int len = n1;
    while(ref(res, disp_bignum_data + (len-1)*wordsize) == 0){
      len--;
      if(len == 0){
        return 0;
      }
    }
    if(result_sign == 0){
      /* positive result */
      if(len == 1){
        unsigned int fst_limb = (unsigned int) ref(res, disp_bignum_data);
        if(fst_limb <= most_positive_fixnum){
          return fix((int)fst_limb);
        }
      }
      ref(res, 0) = (ikp)
                    ((len << bignum_length_shift) |
                     result_sign |
                     bignum_tag);
      return res+vector_tag;
    } else {
      /* negative result */
      if(len == 1){
        unsigned int fst_limb = (unsigned int) ref(res, disp_bignum_data);
        if(fst_limb <= most_negative_fixnum){
          return fix(-(int)fst_limb);
        }
      }
      ref(res, 0) = (ikp)
                    ((len << bignum_length_shift) |
                     result_sign |
                     bignum_tag);
      return res+vector_tag;
    }
  }
}


ikp 
ikrt_fxfxmult(ikp x, ikp y, ikpcb* pcb){
  int n1 = unfix(x);
  int n2 = unfix(y);
  mp_limb_t lo = 0;
  mp_limb_t s1 = n1;
  mp_limb_t s2 = n2;
  int sign = 0;
  if(n1 < 0){
    s1 = -n1;
    sign = 1 - sign;
  } 
  if(n2 < 0){
    s2 = -n2;
    sign = 1 - sign;
  } 
  mp_limb_t hi = mpn_mul_1(&lo, &s1, 1, s2);
  if(hi == 0){
    if(sign){
      if(lo <= most_negative_fixnum){
        return fix(-((int)lo));
      } 
    } else {
      if(lo <= most_positive_fixnum){
        return fix((int)lo);
      }
    }
    ikp r = ik_alloc(pcb, disp_bignum_data + wordsize);
    ref(r, 0) = (ikp)
      (bignum_tag |
       (sign << bignum_sign_shift) |
       (1 << bignum_length_shift));
    ref(r, disp_bignum_data) = (ikp)lo;
    return r+vector_tag;
  } else {
    ikp r = ik_alloc(pcb, align(disp_bignum_data + 2*wordsize));
    ref(r, 0) = (ikp)
      (bignum_tag | 
       (sign << bignum_sign_shift) |
       (2 << bignum_length_shift));
    ref(r, disp_bignum_data) = (ikp)lo;
    ref(r, disp_bignum_data+wordsize) = (ikp)hi;
    return r+vector_tag;
  }
}

ikp
normalize_bignum(int limbs, int sign, ikp r){
  while(ref(r, disp_bignum_data + (limbs-1)*wordsize) == 0){
    limbs--;
    if(limbs == 0){ return 0;}
  }
  if(limbs == 1){
    unsigned int last = (unsigned int) ref(r, disp_bignum_data);
    if(sign == 0){
      if(last <= most_positive_fixnum){
        return fix((int)last);
      }
    } else {
      if(last <= most_negative_fixnum){
        return fix(-((int)last));
      }
    }
  }
  ref(r, 0) = (ikp)
    (bignum_tag |
     sign |
     (limbs << bignum_length_shift));
  return r+vector_tag;
}


ikp 
ikrt_fxbnmult(ikp x, ikp y, ikpcb* pcb){
  int n2 = unfix(x);
  if(n2 == 0) { return 0; }
  mp_limb_t s2 = (n2>0) ? n2 : (- n2);
  ikp fst = ref(y, -vector_tag);
  int limb_count = ((unsigned int) fst) >> bignum_length_shift;
  ikp r = ik_alloc(pcb, align(disp_bignum_data + (limb_count+1)*wordsize));
  mp_limb_t hi = mpn_mul_1((mp_limb_t*)(r+disp_bignum_data),
                           (mp_limb_t*)(y-vector_tag+disp_bignum_data),
                           limb_count,
                           s2);
  ref(r, disp_bignum_data + limb_count * wordsize) = (ikp)hi;
  int sign = 
    ((n2 > 0) ?
     (bignum_sign_mask & (int)fst) : 
     ((1 << bignum_sign_shift) - (bignum_sign_mask&(int)fst)));
  return normalize_bignum(limb_count+1, sign, r);
}

ikp 
ikrt_bnbnmult(ikp x, ikp y, ikpcb* pcb){
  int f1 = (int)ref(x, -vector_tag);
  int f2 = (int)ref(y, -vector_tag);
  int n1 = ((unsigned int)f1) >> bignum_length_shift;
  int n2 = ((unsigned int)f2) >> bignum_length_shift;
  int nr = n1 + n2;
  ikp bn = ik_alloc(pcb, align(disp_bignum_data + nr*wordsize));
  mp_limb_t r;
  if(n1 >= n2){
    r = mpn_mul((mp_limb_t*)(bn+disp_bignum_data),
                (mp_limb_t*)(x-vector_tag+disp_bignum_data),
                n1,
                (mp_limb_t*)(y-vector_tag+disp_bignum_data),
                n2);
  } else {
    r = mpn_mul((mp_limb_t*)(bn+disp_bignum_data),
                (mp_limb_t*)(y-vector_tag+disp_bignum_data),
                n2,
                (mp_limb_t*)(x-vector_tag+disp_bignum_data),
                n1);
  } 
  int sign = 
    ((bignum_sign_mask & f1) ?
     ((1 << bignum_sign_shift) - (bignum_sign_mask & f2)) :
     (bignum_sign_mask & f2));
  return normalize_bignum(nr, sign, bn);
}




ikp
ikrt_bnbncomp(ikp bn1, ikp bn2){
  ikp f1 = ref(bn1, -vector_tag);
  ikp f2 = ref(bn2, -vector_tag);
  if(bignum_sign_mask & (int)f1){
    if(bignum_sign_mask & (int)f2){
      /* both negative */
      int n1 = ((unsigned int) f1) >> bignum_length_shift;
      int n2 = ((unsigned int) f2) >> bignum_length_shift;
      if(n1 < n2) {
        return fix(1);
      } else if(n1 > n2){
        return fix(-1);
      } else {
        int i;
        for(i=(n1-1); i>=0; i--){
          unsigned int t1 = 
            (unsigned int) ref(bn1,disp_bignum_data-vector_tag+i*wordsize);
          unsigned int t2 = 
            (unsigned int) ref(bn2,disp_bignum_data-vector_tag+i*wordsize);
          if(t1 < t2){
            return fix(1);
          } else if(t1 > t2){
            return fix(-1);
          }
        }
      }
      return 0;
    } else {
      /* n1 negative, n2 positive */
      return fix(-1);
    } 
  } else {
    if(bignum_sign_mask & (int)f2){
      /* n1 positive, n2 negative */
      return fix(1);
    } else {
      /* both positive */
      int n1 = ((unsigned int) f1) >> bignum_length_shift;
      int n2 = ((unsigned int) f2) >> bignum_length_shift;
      if(n1 < n2) {
        return fix(-1);
      } else if(n1 > n2){
        return fix(1);
      } else {
        int i;
        for(i=(n1-1); i>=0; i--){
          unsigned int t1 = 
           (unsigned int) ref(bn1,disp_bignum_data-vector_tag+i*wordsize);
          unsigned int t2 = 
            (unsigned int) ref(bn2,disp_bignum_data-vector_tag+i*wordsize);
          if(t1 < t2){
            return fix(-1);
          } else if(t1 > t2){
            return fix(1);
          }
        }
      }
      return 0;
    }
  }
}

ikp
ikrt_fxbnlogand(ikp x, ikp y, ikpcb* pcb){
  int n1 = unfix(x);
  ikp fst = ref(y, -vector_tag);
  if(n1 >= 0){
    if(bignum_sign_mask & (unsigned int) fst){
      /* y is negative */
      return fix(n1 & (1+~(int)ref(y, disp_vector_data-vector_tag))); 
    } else {
      /* y is positive */
      return fix(n1 & (int)ref(y, disp_vector_data-vector_tag)); 
    }
  } else {
    if(n1 == -1){ return y; }
    if(bignum_sign_mask & (unsigned int) fst){
      /* y is negative */
      int len = (((unsigned int) fst) >> bignum_length_shift);
      unsigned int nn = 
        (1+~((1+~(int)ref(y, disp_bignum_data - vector_tag)) & n1));
      if((len == 1) && (nn <= most_negative_fixnum)){
        return fix(-nn);
      }
      ikp r = ik_alloc(pcb, align(disp_bignum_data + len * wordsize));
      ref(r, 0) = fst;
      ref(r, disp_bignum_data) = (ikp) nn;
      int i;
      for(i=1; i<len; i++){
        ref(r, disp_bignum_data+i*wordsize) = 
          ref(y, disp_bignum_data-vector_tag+i*wordsize);
      }
      return r+vector_tag;
    } else {
      /* y is positive */
      int len = (((unsigned int) fst) >> bignum_length_shift);
      ikp r = ik_alloc(pcb, align(disp_bignum_data + len * wordsize));
      ref(r, 0) = fst;
      ref(r, disp_bignum_data) = (ikp)
        (((int)ref(y, disp_bignum_data - vector_tag)) & n1);
      int i;
      for(i=1; i<len; i++){
        ref(r, disp_bignum_data+i*wordsize) = 
          ref(y, disp_bignum_data-vector_tag+i*wordsize);
      }
      return r+vector_tag;
    }
  }
}


static inline int
count_leading_ffs(int n, unsigned int* x){
  int idx;
  for(idx=0; idx<n; idx++){
    if(x[idx] != -1){
      return idx;
    }
  }
  return n;
}

ikp
ikrt_bnbnlogand(ikp x, ikp y, ikpcb* pcb){
  ikp xfst = ref(x, -vector_tag);
  ikp yfst = ref(y, -vector_tag);
  int n1 = ((unsigned int) xfst) >> bignum_length_shift;
  int n2 = ((unsigned int) yfst) >> bignum_length_shift;
  if(bignum_sign_mask & (unsigned int) xfst){
    if(bignum_sign_mask & (unsigned int) yfst){
      fprintf(stderr, "not yet for bnbnlogand\n");
      exit(-1);
    } else {
      return ikrt_bnbnlogand(y,x,pcb);
    }
  } else {
    if(bignum_sign_mask & (unsigned int) yfst){
      /* x positive, y negative */
      /*  the result is at most n1 words long */
      unsigned int* s1 = ((unsigned int*)(x+disp_bignum_data-vector_tag));
      unsigned int* s2 = ((unsigned int*)(y+disp_bignum_data-vector_tag));
      if(n1 <= n2){
        int i = n1-1;
        while(i >= 0){
          unsigned int t = s1[i] & (1+~s2[i]);
          if(t != 0){
            if((i == 0) && (t <= most_positive_fixnum)){
              return fix(t);
            }
            ikp r = ik_alloc(pcb, align(disp_bignum_data+(i+1)*wordsize));
            ref(r, 0) = (ikp) (bignum_tag | ((i+1) << bignum_length_shift));
            unsigned int* s = (unsigned int*)(r+disp_bignum_data);
            s[i] = t;
            for(i--; i>=0; i--){
              s[i] = s1[i] & (1+~s2[i]);
            }
            return r+vector_tag;
          } else {
            i--;
          }
        }
        return 0;
      } else {
        fprintf(stderr, "not yet for bnbnlogand\n");
        exit(-1);
      }
    } else {
      /* both positive */
      int n = (n1<n2)?n1:n2;
      int i;
      for(i=n-1; i>=0; i--){
        int l1 = 
          (int) ref(x, disp_bignum_data-vector_tag+i*wordsize);
        int l2 = 
          (int) ref(y, disp_bignum_data-vector_tag+i*wordsize);
        int last = l1 & l2;
        if(last){
          if((i == 0) && (last < most_positive_fixnum)){
            return fix(last);
          }
          ikp r = ik_alloc(pcb, align(disp_bignum_data+(i+1)*wordsize));
          ref(r, 0) = (ikp) (bignum_tag | ((i+1)<<bignum_length_shift));
          ref(r, disp_bignum_data + i*wordsize) = (ikp)last;
          int j;
          for(j=0; j<i; j++){
            ref(r, disp_bignum_data + j*wordsize) = (ikp)
              (((int)ref(x, disp_bignum_data-vector_tag+j*wordsize))
               &
               ((int)ref(y, disp_bignum_data-vector_tag+j*wordsize)));
          }
          return r+vector_tag;
        }
      }
      return 0;
    }
  }
}


ikp
ikrt_bntostring(ikp x, ikpcb* pcb){
  /* FIXME: avoid calling malloc, instead, use the heap pointer itself
   * as a buffer to hold the temporary data after ensuring that it has enough
   * space */
  ikp fst = ref(x, -vector_tag);
  int limb_count = (((unsigned int)fst) >> bignum_length_shift);
  if(limb_count <= 0){
    fprintf(stderr, "BUG: nbtostring: invalid length %d\n", limb_count);
    exit(-1);
  }
  int sign_bit = bignum_sign_mask & (int) fst;
  int nbsize = limb_count * sizeof(mp_limb_t);
  int strsize = limb_count * max_digits_per_limb;
  int mem_req = nbsize + strsize + 1;
  unsigned char* mem = malloc(mem_req);
  if(! mem){
    fprintf(stderr, "Error allocating space for bignum\n");
    exit(-1);
  }
  memcpy(mem, x - vector_tag + disp_bignum_data, nbsize);
  mp_size_t bytes = 
    mpn_get_str(mem+nbsize,       /* output string */ 
                10,               /* base */
                (mp_limb_t*) mem, /* limb */
                limb_count        /* number of limbs */
        );
  unsigned char* string_start = mem + nbsize;
  while(*string_start == 0){
    string_start++;
    bytes--;
  }
  ikp str = ik_alloc(pcb, align(bytes + disp_string_data + (sign_bit?1:0)));
  ref(str, 0) = fix(bytes + (sign_bit?1:0));
  ikp dest = str + disp_string_data;
  if(sign_bit){
    *dest = '-';
    dest++;
  }
  { 
    int i = 0;
    while(i < bytes){
      dest[i] = string_start[i] + '0';
      i++;
    }
    dest[bytes] = 0;
  }
  free(mem);
  return str + string_tag;
}

