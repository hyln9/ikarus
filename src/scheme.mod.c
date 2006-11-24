/* automatically generated, do not edit */
#include "scheme.h"
#include <stdio.h>
ptr scheme_main(pcb_t* pcb){
   libsymboltable_entry(pcb);
   libhandlers_entry(pcb);
   libcore_entry(pcb);
   //libio_entry(pcb);
   //libwriter_entry(pcb);
   //libtokenizer_entry(pcb);
   //libeval_entry(pcb);
   //libcafe_entry(pcb);
   //libtoplevel_entry(pcb);
   return scheme_entry(pcb);
}
