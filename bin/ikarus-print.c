

#include "ikarus.h"
#include <stdio.h>
#include <stdlib.h>

static void print(FILE* fh, ikp x);

void ik_fprint(FILE* fh, ikp x){
  print(fh, x);
}

void ik_print(ikp x){
  print(stdout, x);
  fprintf(stdout, "\n");
}

static char* char_string[128] = {
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
print(FILE* fh, ikp x){
  if(IK_FIXNUMP(x)){
    fprintf(fh, "%d", IK_UNFIX(x));
  }
  else if(x == IK_FALSE_OBJECT){
    fprintf(fh, "#f");
  }
  else if(x == IK_TRUE_OBJECT){
    fprintf(fh, "#t");
  }
  else if(x == IK_NULL_OBJECT){
    fprintf(fh, "()");
  }
  else if(IK_CHARP(x)){
    fprintf(fh, "%s", char_string[IK_CHAR_VAL(x)]);
  }
#if 0
  else if(tagof(x) == symbol_tag){
    ikp str = ref(x, off_symbol_string);
    fprintf(fh, "%s", str+off_string_data);
  }
#endif
  else if(tagof(x) == vector_tag){
    ikp len = ref(x, off_vector_length);
    if(len == 0){
      fprintf(fh, "#()");
    } else {
      fprintf(fh, "#(");
      ikp data = x + off_vector_data;
      print(fh, ref(data, 0));
      ikp i = (ikp)wordsize;
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
  else if(IK_PAIRP(x)){
    fprintf(fh, "(");
    print(fh, REF(x, IK_OFF_CAR));
    ikp d = REF(x, IK_OFF_CDR);
    fprintf(stderr, "d=0x%08x\n", (int)d);
    while(1){
      if(IK_PAIRP(d)){
        fprintf(fh, " ");
        print(fh, REF(d, IK_OFF_CAR));
        d = REF(d, IK_OFF_CDR);
      } 
      else if(d == IK_NULL_OBJECT){
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
    ikp fxlen = ref(x, off_string_length);
    int len = unfix(fxlen);
    fprintf(stderr, "bug: printer busted!\n");
    exit(-1);
    char* data = 0; //string_data(x);
    fprintf(fh, "\"");
    int i;
    for(i=0; i<len; i++){
      char c = data[i];
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


