/*
 *  Ikarus Scheme -- A compiler for R6RS Scheme.
 *  Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
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
#include <stdio.h>
#include <stdlib.h>

static void print(FILE* fh, ikptr x);

void ik_fprint(FILE* fh, ikptr x){
  print(fh, x);
}

void ik_print(ikptr x){
  print(stdout, x);
  fprintf(stdout, "\n");
}

char* char_string[128] = {
  "#\\nul","#\\soh","#\\stx","#\\etx","#\\eot","#\\enq","#\\ack","#\\bel",
  "#\\bs", "#\\tab","#\\newline", "#\\vt", "#\\ff", "#\\return", "#\\so",
  "#\\si",
  "#\\dle","#\\dc1","#\\dc2","#\\dc3","#\\dc4","#\\nak","#\\syn","#\\etb",
  "#\\can","#\\em", "#\\sub","#\\esc","#\\fs", "#\\gs", "#\\rs", "#\\us",
  "#\\space","#\\!","#\\\"","#\\#","#\\$","#\\%","#\\&","#\\'",
  "#\\(","#\\)","#\\*","#\\+","#\\,","#\\-","#\\.","#\\/",
  "#\\0","#\\1","#\\2","#\\3","#\\4","#\\5","#\\6","#\\7",
  "#\\8","#\\9","#\\:","#\\;","#\\<","#\\=","#\\>","#\\?",
  "#\\@","#\\A","#\\B","#\\C","#\\D","#\\E","#\\F","#\\G",
  "#\\H","#\\I","#\\J","#\\K","#\\L","#\\M","#\\N","#\\O",
  "#\\P","#\\Q","#\\R","#\\S","#\\T","#\\U","#\\V","#\\W",
  "#\\X","#\\Y","#\\Z","#\\[","#\\\\","#\\]","#\\^","#\\_",
  "#\\`","#\\a","#\\b","#\\c","#\\d","#\\e","#\\f","#\\g",
  "#\\h","#\\i","#\\j","#\\k","#\\l","#\\m","#\\n","#\\o",
  "#\\p","#\\q","#\\r","#\\s","#\\t","#\\u","#\\v","#\\w",
  "#\\x","#\\y","#\\z","#\\{","#\\|","#\\}","#\\~","#\\del"};




static void
print(FILE* fh, ikptr x){
  if(is_fixnum(x)){
    fprintf(fh, "%ld", unfix(x));
  }
  else if(x == false_object){
    fprintf(fh, "#f");
  }
  else if(x == true_object){
    fprintf(fh, "#t");
  }
  else if(x == null_object){
    fprintf(fh, "()");
  }
  else if(is_char(x)){
    unsigned long int i = ((long int)x) >> char_shift;
    if(i < 128){
      fprintf(fh, "%s", char_string[i]);
    } else {
      fprintf(fh, "#\\x%lx", i);
    }
  }
#if 0
  else if(tagof(x) == symbol_tag){
    ikptr str = ref(x, off_symbol_string);
    fprintf(fh, "%s", str+off_string_data);
  }
#endif
  else if(tagof(x) == vector_tag){
    ikptr len = ref(x, off_vector_length);
    if(len == 0){
      fprintf(fh, "#()");
    } else {
      fprintf(fh, "#(");
      ikptr data = x + off_vector_data;
      print(fh, ref(data, 0));
      ikptr i = (ikptr)wordsize;
      while(i<len){
        fprintf(fh, " ");
        print(fh, ref(data,i));
        i += wordsize;
      }
      fprintf(fh, ")");
    }
  }
  else if(is_closure(x)){
    fprintf(fh, "#<procedure>");
  }
  else if(is_pair(x)){
    fprintf(fh, "(");
    print(fh, ref(x, off_car));
    ikptr d = ref(x, off_cdr);
    /* fprintf(stderr, "d=0x%016lx\n", (long int)d); */
    while(1){
      if(is_pair(d)){
        fprintf(fh, " ");
        print(fh, ref(d, off_car));
        d = ref(d, off_cdr);
      } 
      else if(d == null_object){
        fprintf(fh, ")");
        return;
      }
      else {
        fprintf(fh, " . ");
        print(fh, d);
        fprintf(fh, ")");
        return;
      }
    }
  }
  else if(tagof(x) == string_tag){
    ikptr fxlen = ref(x, off_string_length);
    int len = unfix(fxlen);
    int * data = (int*)(x + off_string_data);
    fprintf(fh, "\"");
    int i;
    for(i=0; i<len; i++){
      char c = (data[i]) >> char_shift;
      if((c == '\\') || (c == '"')){
        fprintf(fh, "\\");
      }
      fprintf(fh, "%c", c);
    }
    fprintf(fh, "\"");
  }
  else {
    fprintf(fh, "#<unknown>");
  }
}


