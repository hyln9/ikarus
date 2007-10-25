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

ikp
ikrt_weak_cons(ikp a, ikp d, ikpcb* pcb){
  ikp ap = pcb->weak_pairs_ap;
  ikp nap = ap + pair_size;
  ikp p;
  if(nap > pcb->weak_pairs_ep){
    ikp mem = ik_mmap_typed(pagesize, weak_pairs_mt, pcb);
    pcb->weak_pairs_ap = mem + pair_size;
    pcb->weak_pairs_ep = mem + pagesize;
    p = mem + pair_tag;
  }
  else {
    pcb->weak_pairs_ap = nap;
    p = ap + pair_tag;
  }
  ref(p, off_car) = a;
  ref(p, off_cdr) = d;
  return p;
}

ikp
ikrt_is_weak_pair(ikp x, ikpcb* pcb){
  if(tagof(x) != pair_tag){
    return false_object;
  }
  unsigned int t = pcb->segment_vector[page_index(x)];
  if((t & type_mask) == weak_pairs_type){
    return true_object;
  } else {
    return false_object;
  }
}


