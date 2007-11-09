/*
 *  Ikarus Scheme -- A compiler for R6RS Scheme.
 *  Copyright (C) 2006,2007  Abdulaziz Ghuloum
 *  
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License version 3 as
 *  published by the Free Software Foundation.
 *  
 *  This program is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  General Public License for more details.
 *  
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */



#include "ikarus-data.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <gmp.h>


#define most_positive_fixnum 0x1FFFFFFF
#define most_negative_fixnum 0x20000000

#define max_digits_per_limb 10

#ifdef NDEBUG
#define verify_bignum(x,caller) (x)
#else
static ikp
verify_bignum(ikp x, char* caller){
  if(tagof(x) != vector_tag){
    fprintf(stderr, "Error in (%s) invalid primary tag %p\n", caller, x);
    exit(-1);
  }
  ikp fst = ref(x, -vector_tag);
  int limb_count = ((unsigned int) fst) >> bignum_length_shift;
  if(limb_count <= 0){
    fprintf(stderr, 
        "Error in (%s) invalid limb count in fst=0x%08x\n", 
        caller, (int)fst);
    exit(-1);
  }
  int pos;
  if((int)fst & bignum_sign_mask){
    pos = 0;
  } else {
    pos = 1;
  }
  unsigned int last_limb = 
    (unsigned int) ref(x, off_bignum_data + (limb_count - 1) * wordsize);
  if(last_limb == 0){
    fprintf(stderr, 
        "Error in (%s) invalid last limb = 0x%08x", caller, last_limb);
    exit(-1);
  }
  if(limb_count == 1){
    if(pos){
      if(last_limb <= most_positive_fixnum){
        fprintf(stderr, 
                "Error in (%s) should be a positive fixnum: 0x%08x\n", 
                caller, last_limb);
        exit(-1);
      }
    } else {
      if(last_limb <= most_negative_fixnum){
        fprintf(stderr, 
                "Error in (%s) should be a negative fixnum: 0x%08x\n", 
                caller, last_limb);
        exit(-1);
      }
    }
  }
  /* ok */
  return x;
}
#endif

#define BN(x) verify_bignum(x,"BN")

#if 0
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
#endif

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
ikrt_even_bn(ikp x){
  int fst = (int)ref(x, wordsize-vector_tag);
  if(fst & 1){
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
    return verify_bignum(bn+vector_tag, "fxfx+");
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
        return verify_bignum(r+vector_tag, "fxbn+1");
      } else {
        ref(r, 0) = (ikp)
          ((limb_count << bignum_length_shift) |
           (0 << bignum_sign_shift) |
           bignum_tag);
        return verify_bignum(r+vector_tag, "fxbn+2");
      }
    }
    else {
      //fprintf(stderr, "this case 0x%08x\n", intx);
      /* positive fx + negative bn = smaller negative bn */
      ikp r = ik_alloc(pcb, align(disp_bignum_data+limb_count*wordsize));
      int borrow = mpn_sub_1((mp_limb_t*)(r+disp_bignum_data), 
                             (mp_limb_t*)(y - vector_tag + disp_bignum_data),
                             limb_count,
                             intx);
      if(borrow){
        fprintf(stderr, "Error: BUG in borrow1 %d\n", borrow);
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
      return verify_bignum(r+vector_tag, "fxbn+3");
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
        fprintf(stderr, "Error: BUG in borrow2\n");
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
      return verify_bignum(r+vector_tag, "fxbn+4");
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
        return verify_bignum(r+vector_tag, "fxbn+5");
      } else {
        ref(r, 0) = (ikp)
          ((limb_count << bignum_length_shift) |
           (1 << bignum_sign_shift) |
           bignum_tag);
        return verify_bignum(r+vector_tag, "fxbn+5");
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
      return verify_bignum(res+vector_tag, "bnbn+1");
    } else {
      ref(res, 0) = (ikp)
                    ((n1 << bignum_length_shift) |
                     xsign |
                     bignum_tag);
      return verify_bignum(res+vector_tag, "bnbn+2");
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
      return verify_bignum(res+vector_tag, "bnbn+3");
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
      return verify_bignum(res+vector_tag, "bnbn+4");
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
      return verify_bignum(bn+vector_tag,"fxfx-1");
    }
  } else {
    ikp fxr = fix(r);
    if(unfix(fxr) == r){
      return fxr;
    } else {
      ikp bn = ik_alloc(pcb, align(disp_bignum_data + wordsize));
      ref(bn, 0) = (ikp) 
        (bignum_tag | 
         (1 << bignum_sign_shift) |
         (1 << bignum_length_shift));
      ref(bn, disp_bignum_data) = (ikp)(-r);
      return verify_bignum(bn+vector_tag, "fxfx-2");
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
  return verify_bignum(bn+vector_tag, "bnneg");
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
        return verify_bignum(r+vector_tag, "fxbn-1");
      } else {
        ref(r, 0) = (ikp)
          ((limb_count << bignum_length_shift) |
           (0 << bignum_sign_shift) |
           bignum_tag);
        return verify_bignum(r+vector_tag, "fxbn-2");
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
        fprintf(stderr, "Error: BUG in borrow3\n");
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
      return verify_bignum(r+vector_tag, "fxbn-");
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
        fprintf(stderr, "Error: BUG in borrow4\n");
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
      return verify_bignum(r+vector_tag,"fxbn-");
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
        return verify_bignum(r+vector_tag, "fxbn-");
      } else {
        ref(r, 0) = (ikp)
          ((limb_count << bignum_length_shift) |
           (1 << bignum_sign_shift) |
           bignum_tag);
        return verify_bignum(r+vector_tag, "fxbn-");
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
        return verify_bignum(r+vector_tag,"bnfx-");
      } else {
        ref(r, 0) = (ikp)
          ((limb_count << bignum_length_shift) |
           (0 << bignum_sign_shift) |
           bignum_tag);
        return verify_bignum(r+vector_tag,"bnfx-");
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
        fprintf(stderr, "Error: BUG in borrow5\n");
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
      return verify_bignum(r+vector_tag,"bnfx-");
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
        fprintf(stderr, "Error: BUG in borrow6\n");
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
      return verify_bignum(r+vector_tag, "bnfx-");
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
        return verify_bignum(r+vector_tag, "bnfx-");
      } else {
        ref(r, 0) = (ikp)
          ((limb_count << bignum_length_shift) |
           (1 << bignum_sign_shift) |
           bignum_tag);
        return verify_bignum(r+vector_tag, "bnfx-");
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
      return verify_bignum(res+vector_tag, "bnbn-");
    } else {
      ref(res, 0) = (ikp)
                    ((n1 << bignum_length_shift) |
                     xsign |
                     bignum_tag);
      return verify_bignum(res+vector_tag, "bnbn-");
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
          result_sign = (1 << bignum_sign_shift) - ysign;
        }
      } else {
        s1 = y; n1 = ylimbs;
        s2 = x; n2 = xlimbs;
        result_sign = (1 << bignum_sign_shift) - ysign;
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
      return verify_bignum(res+vector_tag, "bnbn-");
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
      return verify_bignum(res+vector_tag, "bnbn-");
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
    return BN(r+vector_tag);
  } else {
    ikp r = ik_alloc(pcb, align(disp_bignum_data + 2*wordsize));
    ref(r, 0) = (ikp)
      (bignum_tag | 
       (sign << bignum_sign_shift) |
       (2 << bignum_length_shift));
    ref(r, disp_bignum_data) = (ikp)lo;
    ref(r, disp_bignum_data+wordsize) = (ikp)hi;
    return BN(r+vector_tag);
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
  ref(r, 0) = (ikp) (bignum_tag | sign | (limbs << bignum_length_shift));
  return BN(r+vector_tag);
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

/* FIXME: Too complicated! */
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
      return BN(r+vector_tag);
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
      return BN(r+vector_tag);
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

static void
bits_compliment(unsigned int* src, unsigned int* dst, int n){
  int carry = 1;
  int i;
  for(i=0; i<n; i++){
    unsigned int d = src[i];
    unsigned int c = carry + ~ d;
    dst[i] = c;
    carry = (carry && ! d);
  }
}

static void
bits_compliment_with_carry(unsigned int* src, unsigned int* dst, int
    n, int carry){
  int i;
  for(i=0; i<n; i++){
    unsigned int d = src[i];
    unsigned int c = carry + ~ d;
    dst[i] = c;
    carry = (carry && ! d);
  }
}

static void
bits_compliment_logand(unsigned int* s1, unsigned int* s2, unsigned int* dst, int n){
  int carry = 1;
  int i;
  for(i=0; i<n; i++){
    unsigned int d = s1[i];
    unsigned int c = carry + ~ d;
    dst[i] = c & s2[i];
    carry = (carry && ! d);
  }
}

static int
bits_carry(unsigned int* s, int n){
  /*
  int carry = 1;
  int i;
  for(i=0; i<n; i++){
    unsigned int d = s[i];
    carry = (carry && ! d);
  }
  return carry;
  */
  int i;
  for(i=0; i<n; i++){
    if (s[i] != 0){
      return 0;
    }
  }
  return 1;
}

ikp
ikrt_bnlognot(ikp x, ikpcb* pcb){
  ikp fst = ref(x, -vector_tag);
  int n = ((unsigned int)fst) >> bignum_length_shift;
  unsigned int* s1 = ((unsigned int*)(x+disp_bignum_data-vector_tag));
  if(bignum_sign_mask & (unsigned int) fst){
    /* negative */
    ikp r = ik_alloc(pcb, align(disp_bignum_data + n*wordsize));
    unsigned int* rd = (unsigned int*)(r+disp_bignum_data);
    int i;
    for(i=0; (i<n) && (s1[i] == 0); i++) {
      rd[i] = -1;
    }
    rd[i] = s1[i] - 1;
    for(i++; i<n; i++){
      rd[i] = s1[i];
    }
    return normalize_bignum(n, 0, r);
  } else {
    /* positive */
    int i;
    for(i=0; (i<n) && (s1[i] == -1); i++) {/*nothing*/}
    if(i==n){
      ikp r = ik_alloc(pcb, align(disp_bignum_data + (n+1)*wordsize));
      bzero(r+disp_bignum_data, n*wordsize);
      ((unsigned int*)(r+disp_bignum_data))[n] = 1;
      ref(r, 0) = (ikp)
        (bignum_tag | (1<<bignum_sign_shift) | ((n+1) << bignum_length_shift));
      return r+vector_tag;
    } else {
      ikp r = ik_alloc(pcb, align(disp_bignum_data + n*wordsize));
      unsigned int* rd = (unsigned int*)(r+disp_bignum_data);
      int j;
      for(j=0; j<i; j++){ rd[j] = 0; }
      rd[i] = s1[i] + 1;
      for(j=i+1; j<n; j++){ rd[j] = s1[j]; }
      ref(r, 0) = (ikp)
        (bignum_tag | (1<<bignum_sign_shift) | (n << bignum_length_shift));
      return r+vector_tag;
    }
  }
}


ikp
ikrt_bnbnlogand(ikp x, ikp y, ikpcb* pcb){
  ikp xfst = ref(x, -vector_tag);
  ikp yfst = ref(y, -vector_tag);
  int n1 = ((unsigned int) xfst) >> bignum_length_shift;
  int n2 = ((unsigned int) yfst) >> bignum_length_shift;
  if(bignum_sign_mask & (unsigned int) xfst){
    if(bignum_sign_mask & (unsigned int) yfst){
      unsigned int* s1 = ((unsigned int*)(x+disp_bignum_data-vector_tag));
      unsigned int* s2 = ((unsigned int*)(y+disp_bignum_data-vector_tag));
      if(n1 >= n2){
        ikp r = ik_alloc(pcb, align(disp_bignum_data + n1*wordsize));
        unsigned int* s = ((unsigned int*)(r+disp_bignum_data));
        bits_compliment(s1, s, n1);
        bits_compliment_logand(s2, s, s, n2);
        bits_compliment(s, s, n1);
        return normalize_bignum(n1, 1<<bignum_sign_shift, r);
      } else {
        return ikrt_bnbnlogand(y,x,pcb);
      }
    } else {
      return ikrt_bnbnlogand(y,x,pcb);
    }
  } else {
    if(bignum_sign_mask & (unsigned int) yfst){
      /* x positive, y negative */
      /*  the result is at most n1 words long */
      unsigned int* s1 = ((unsigned int*)(x+disp_bignum_data-vector_tag));
      unsigned int* s2 = ((unsigned int*)(y+disp_bignum_data-vector_tag));
      ikp r = ik_alloc(pcb, align(disp_bignum_data + n1*wordsize));
      unsigned int* s = ((unsigned int*)(r+disp_bignum_data));
      bits_compliment_logand(s2, s1, s, n1);
      return normalize_bignum(n1, 0, r);
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


static void
copy_bits_shifting_right(unsigned int* src, unsigned int* dst, int n, int m){
  unsigned int carry = src[0] >> m;
  int i;
  for(i=1; i<n; i++){
    unsigned int b = src[i];
    dst[i-1] = (b << (32-m)) | carry;
    carry = b >> m;
  }
  dst[n-1] = carry;
}

static void
copy_bits_shifting_left(unsigned int* src, unsigned int* dst, int n, int m){
  unsigned int carry = 0;
  int i;
  for(i=0; i<n; i++){
    unsigned int b = src[i];
    dst[i] = (b << m) | carry;
    carry = b >> (32-m);
  }
  dst[n] = carry;
}





ikp
ikrt_bignum_shift_right(ikp x, ikp y, ikpcb* pcb){
  int m = unfix(y);
  ikp fst = ref(x, -vector_tag);
  int n = ((unsigned int) fst) >> bignum_length_shift;
  int whole_limb_shift = m >> 5; /* FIXME: 5 are the bits in 32-bit num */
  int bit_shift = m & 31;
  int new_limb_count = n - whole_limb_shift;
  if(bignum_sign_mask & (unsigned int) fst){
    if(new_limb_count <= 0){
      return fix(-1);
    }
    if(bit_shift == 0){
      ikp r = ik_alloc(pcb, align(disp_bignum_data + new_limb_count * wordsize));
      bits_compliment_with_carry(
          (unsigned int*)(x+off_bignum_data+whole_limb_shift*wordsize),
          (unsigned int*)(r+disp_bignum_data),
          new_limb_count,
          bits_carry((unsigned int*)(x+off_bignum_data), whole_limb_shift));
      bits_compliment(
          (unsigned int*)(r+disp_bignum_data),
          (unsigned int*)(r+disp_bignum_data),
          new_limb_count);
      return normalize_bignum(new_limb_count, 1 << bignum_sign_shift, r);
    } else {
      ikp r = ik_alloc(pcb, align(disp_bignum_data + new_limb_count * wordsize));
      bits_compliment_with_carry(
          (unsigned int*)(x+off_bignum_data+whole_limb_shift*wordsize),
          (unsigned int*)(r+disp_bignum_data),
          new_limb_count,
          bits_carry((unsigned int*)(x+off_bignum_data), whole_limb_shift));
      copy_bits_shifting_right(
          (unsigned int*)(r+disp_bignum_data),
          (unsigned int*)(r+disp_bignum_data),
          new_limb_count,
          bit_shift);
      *((unsigned int*)(r+disp_bignum_data+(new_limb_count-1)*wordsize))
          |= (-1 << (32 - bit_shift));
      bits_compliment(
          (unsigned int*)(r+disp_bignum_data),
          (unsigned int*)(r+disp_bignum_data),
          new_limb_count);
      return normalize_bignum(new_limb_count, 1 << bignum_sign_shift, r);
      fprintf(stderr, "not yet for negative bignum_shift\n");
      exit(-1);
    }
  } else {
    if(new_limb_count <= 0){
      return 0;
    }
    if(bit_shift == 0){
      ikp r = ik_alloc(pcb, align(disp_bignum_data + new_limb_count * wordsize));
      memcpy(r+disp_bignum_data,
              x+off_bignum_data+whole_limb_shift*wordsize,
              new_limb_count * wordsize);
      return normalize_bignum(new_limb_count, 0, r);
    } else {
      ikp r = ik_alloc(pcb, align(disp_bignum_data + new_limb_count * wordsize));
      copy_bits_shifting_right(
          (unsigned int*)(x+off_bignum_data+whole_limb_shift*wordsize),
          (unsigned int*)(r+disp_bignum_data),
          new_limb_count,
          bit_shift);
      return normalize_bignum(new_limb_count, 0, r);
    }
  }
}


ikp
ikrt_fixnum_shift_left(ikp x, ikp y, ikpcb* pcb){
  int m = unfix(y);
  int n = unfix(x);
  int limb_count = (m >> 5) + 2; /* FIXME: 5 are the bits in 32-bit num */
  int bit_shift = m & 31;
  ikp r = ik_alloc(pcb, align(disp_bignum_data + limb_count * wordsize));
  unsigned int* s = (unsigned int*)(r+disp_bignum_data);
  bzero(s, limb_count * wordsize);
  if(n >= 0){
    if(bit_shift){
      s[limb_count-1] = n >> (32 - bit_shift);
    }
    s[limb_count-2] = n << bit_shift;
  } else {
    if(bit_shift){
      s[limb_count-1] = (-n) >> (32 - bit_shift);
    }
    s[limb_count-2] = (-n) << bit_shift;
  }
  return normalize_bignum(limb_count, (n>=0)?(0):(1<<bignum_sign_shift), r);
}


ikp
ikrt_bignum_shift_left(ikp x, ikp y, ikpcb* pcb){
  int m = unfix(y);
  ikp fst = ref(x, -vector_tag);
  int n = ((unsigned int) fst) >> bignum_length_shift;
  int whole_limb_shift = m >> 5; /* FIXME: 5 are the bits in 32-bit num */
  int bit_shift = m & 31;
  if(bit_shift == 0){
    int limb_count = n + whole_limb_shift;
    ikp r = ik_alloc(pcb, align(disp_bignum_data + limb_count * wordsize));
    unsigned int* s = (unsigned int*)(r+disp_bignum_data);
    bzero(s, whole_limb_shift*wordsize);
    memcpy(s+whole_limb_shift, x+off_bignum_data, n*wordsize);
    return normalize_bignum(limb_count, (unsigned int)fst & bignum_sign_mask, r);
  } else {
    int limb_count = n + whole_limb_shift + 1;
    ikp r = ik_alloc(pcb, align(disp_bignum_data + limb_count * wordsize));
    unsigned int* s = (unsigned int*)(r+disp_bignum_data);
    bzero(s, whole_limb_shift*wordsize);
    copy_bits_shifting_left(
        (unsigned int*)(x+off_bignum_data),
        s+whole_limb_shift,
        n,
        bit_shift);
    return normalize_bignum(limb_count, (unsigned int)fst & bignum_sign_mask, r);
  }
}


#if 0
From TFM:
void
mpn_tdiv_qr (
  mp limb t *qp,        /* quotient placed here */
  mp limb t *rp,        /* remainder placed here */
  mp size t qxn,        /* must be zero! */ 
  const mp limb t *np,  /* first number  */
  mp size t nn,         /* its length    */
  const mp limb t *dp,  /* second number */
  mp size t dn          /* its length    */
)

Divide {np, nn} by {dp, dn} and put the quotient at {qp,nn-dn+1}
and the remainder at {rp, dn}. The quotient is rounded towards 0.
No overlap is permitted between arguments. nn must be greater than
or equal to dn. The most significant limb of dp must be non-zero.
The qxn operand must be zero. 
#endif

ikp
ikrt_bnbndivrem(ikp x, ikp y, ikpcb* pcb){
  ikp xfst = ref(x, -vector_tag);
  ikp yfst = ref(y, -vector_tag);
  mp_size_t xn = ((unsigned int) xfst) >> bignum_length_shift;
  mp_size_t yn = ((unsigned int) yfst) >> bignum_length_shift;
  if(xn < yn){
    /* quotient is zero, remainder is x */
    ikp rv = ik_alloc(pcb, pair_size);
    ref(rv, disp_car) = 0;
    ref(rv, disp_cdr) = x;
    return rv+pair_tag;
  }
  mp_size_t qn = xn - yn + 1;
  mp_size_t rn = yn;
  ikp q = ik_alloc(pcb, align(disp_bignum_data + qn*wordsize));
  ikp r = ik_alloc(pcb, align(disp_bignum_data + rn*wordsize));
  mpn_tdiv_qr (
      (mp_limb_t*)(q+disp_bignum_data),
      (mp_limb_t*)(r+disp_bignum_data),
      0,
      (mp_limb_t*)(x+off_bignum_data),
      xn,
      (mp_limb_t*)(y+off_bignum_data),
      yn);

  if(((unsigned int) xfst) & bignum_sign_mask){
    /* x is negative => remainder is negative */
    r = normalize_bignum(rn, 1 << bignum_sign_shift, r);
  } else {
    r = normalize_bignum(rn, 0, r);
  }

  if(((unsigned int) yfst) & bignum_sign_mask){
    /* y is negative => quotient is opposite of x */
    int sign = 
      bignum_sign_mask - (((unsigned int)xfst) & bignum_sign_mask);
    q = normalize_bignum(qn, sign, q);
  } else {
    /* y is positive => quotient is same as x */
    int sign = (((unsigned int)xfst) & bignum_sign_mask);
    q = normalize_bignum(qn, sign, q);
  }
  ikp rv = ik_alloc(pcb, pair_size);
  ref(rv, disp_car) = q;
  ref(rv, disp_cdr) = r;
  return rv+pair_tag;
}


#if 0
[Function]

mp_limb_t
mpn_divrem_1 (
  mp limb t *r1p,
  mp size t qxn, 
  mp limb t *s2p,
  mp size t s2n,
  mp limb t s3limb 
) 

Divide {s2p, s2n} by s3limb, and write the quotient at r1p. Return the remainder. 
The integer quotient is written to {r1p+qxn, s2n} and in addition qxn fraction limbs are 
developed and written to {r1p, qxn}. Either or both s2n and qxn can be zero. For most 
usages, qxn will be zero. 
#endif

ikp
ikrt_bnfxdivrem(ikp x, ikp y, ikpcb* pcb){
  int yint = unfix(y);
  mp_limb_t* s2p = (mp_limb_t*)(x+off_bignum_data);
  ikp fst = ref(x, -vector_tag);
  mp_size_t s2n = ((unsigned int) fst) >> bignum_length_shift;
  ikp quot = ik_alloc(pcb,
      align(s2n*wordsize + disp_bignum_data));
  mp_limb_t rv = mpn_divrem_1(
      (mp_limb_t*)(quot+disp_bignum_data),
      0,
      s2p,
      s2n,
      abs(yint));

  ikp rem;

  if(yint < 0){
    /* y is negative => quotient is opposite of x */
    int sign = 
      bignum_sign_mask - (((unsigned int)fst) & bignum_sign_mask);
    quot = normalize_bignum(s2n, sign, quot);
  } else {
    /* y is positive => quotient is same as x */
    int sign = (((unsigned int)fst) & bignum_sign_mask);
    quot = normalize_bignum(s2n, sign, quot);
  }

  /* the remainder is always less than |y|, so it will
     always be a fixnum.  (if y == most_negative_fixnum, 
     then |remainder| will be at most most_positive_fixnum). */
  if(((unsigned int) fst) & bignum_sign_mask){
    /* x is negative => remainder is negative */
    rem = (ikp) -(rv << fx_shift);
  } else {
    rem = fix(rv);
  }
  ikp p = ik_alloc(pcb, pair_size);
  ref(p, disp_car) = quot;
  ref(p, disp_cdr) = rem;
  return p+pair_tag;
}

ikp
ikrt_bnfx_modulo(ikp x, ikp y, ikpcb* pcb){
  int yint = unfix(y);
  mp_limb_t* s2p = (mp_limb_t*)(x+off_bignum_data);
  ikp fst = ref(x, -vector_tag);
  mp_size_t s2n = ((unsigned int) fst) >> bignum_length_shift;
  if(yint < 0){
    if(((unsigned int) fst) & bignum_sign_mask){
      /* x negative, y negative */
      mp_limb_t m = mpn_mod_1(s2p, s2n, -yint);
      return fix(-m);
    } else {
      /* x positive, y negative */
      mp_limb_t m = mpn_mod_1(s2p, s2n, -yint);
      return fix(yint+m);
    }
  } else {
    if(((unsigned int) fst) & bignum_sign_mask){
      /* x negative, y positive */
      mp_limb_t m = mpn_mod_1(s2p, s2n, yint);
      return fix(yint-m);
    } else {
      /* x positive, y positive */
      mp_limb_t m = mpn_mod_1(s2p, s2n, yint);
      return fix(m);
    }
  }
}





ikp
ikrt_bignum_to_bytevector(ikp x, ikpcb* pcb){
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
  ikp bv = ik_alloc(pcb, align(bytes + disp_bytevector_data + (sign_bit?1:0)));
  ref(bv, 0) = fix(bytes + (sign_bit?1:0));
  ikp dest = bv + disp_bytevector_data;
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
  return bv + bytevector_tag;
}


ikp 
ikrt_fxrandom(ikp x){
  int mask = 1; 
  int n = unfix(x); 
  {
    while(mask < n){
      mask = (mask << 1) | 1;
    }
  }
  while(1){
    long r = random() & mask;
    if(r < n){
      return fix(r);
    }
  }
}

