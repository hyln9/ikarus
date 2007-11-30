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

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include "bootfileloc.h"

int main(int argc, char** argv){
  if(argc >= 2){
    char** a = calloc(argc+1, sizeof(char*));
    if(! a) {
      fprintf(stderr, "Error in scheme-script: cannot calloc\n");
      exit(-1);
    }
    a[0] = EXEFILE;
    a[1] = "--r6rs-script";
    int i;
    for(i=1; i<argc; i++){
      a[i+1] = argv[i];
    }
    int rv = execv(EXEFILE, a);
    fprintf(stderr, "Error executing ikarus from scheme-script: %s\n",
        strerror(errno));
    exit(-1);
  } else {
    fprintf(stderr, 
      "Error in scheme-script: you must provide a script name as an argument\n");
    exit(-1);
  }
}

