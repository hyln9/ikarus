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



#include "ikarus-main.h"
#include "bootfileloc.h"
#include <stdio.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

void ikarus_usage_short(){
  fprintf(stderr, "ikarus -h for more help\n");
}

void ikarus_usage(){
  static char* helpstring = 
"\n\
Options for running ikarus scheme:\n\
\n  ikarus -h\n\
    Prints this help message then exits.\n\
\n  ikarus [-b <bootfile>] --r6rs-script <scriptfile> opts ...\n\
    Starts ikarus in r6rs-script mode.  The script file is treated\n\
    as an R6RS-script.  The options opts ... can be obtained using\n\
    the \"command-line\" procedure in the (rnrs programs) library.\n\
\n  ikarus [-b <bootfile>] <file> ... [-- opts ...]\n\
    Starts ikarus in interactive mode.  Each of the files is first\n\
    loaded into the interaction environment before the interactive\n\
    repl is started.  The options opts can be obtained using the\n\
    \"command-line\" procedure.\n\
  \n\
  If the option [-b <bootfile>] is provided, the bootfile is used\n\
  as the system's initial boot file from which the environment is\n\
  initialized.  If the -b option is not supplied, the default boot\n\
  file is used.  The current default boot file location is\n\
  \"%s\".\n\
  Consult the Ikarus Scheme User's Guide for more details.\n\n";
  fprintf(stderr, helpstring, BOOTFILE);
}


/* get_option
   
   takes pointers to argc and argv and looks for the first
   option matching opt.  If one exists, it removes it from the argv
   list, updates argc, and returns a pointer to the option value.
   returns null if option is not found.
   */
char* 
get_option(char* opt, int argc, char** argv){
  int i;
  for(i=1; i<argc; i++){
    if(strcmp(opt, argv[i]) == 0){
      if((i+1) < argc){
        char* rv = argv[i+1];
        int j;
        for(j=i+2; j<argc; j++, i++){
          argv[i] = argv[j];
        }
        return rv;
      } 
      else {
        fprintf(stderr, 
                "ikarus error: option %s requires a value, none provided\n",
                opt);
        ikarus_usage_short();
        exit(-1);
      }
    }
    else if(strcmp("--", argv[i]) == 0){
      return 0;
    }
  }
  return 0;
}

int
get_option0(char* opt, int argc, char** argv){
  int i;
  for(i=1; i<argc; i++){
    if(strcmp(opt, argv[i]) == 0){
      int j;
      for(j=i+1; j<argc; j++, i++){
          argv[i] = argv[j];
        }
      return 1;
    } 
    else if(strcmp("--", argv[i]) == 0){
      return 0;
    }
  }
  return 0;
}

int main(int argc, char** argv){
  if(get_option0("-h", argc, argv)){
    ikarus_usage();
    exit(0);
  }
  char* boot_file = get_option("-b", argc, argv);
  if(boot_file){
    argc -= 2;
  } else {
    boot_file = BOOTFILE;
  }
  return ikarus_main(argc, argv, boot_file);
}


