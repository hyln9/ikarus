#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>

#include "ikarus-data.h"

ikp 
ikrt_io_error(){
  switch(errno){
    case EBADF           : return fix(-2);
    case EINTR           : return fix(-3);
    case ENOTDIR         : return fix(-4);
    case ENAMETOOLONG    : return fix(-5);
    case ENOENT          : return fix(-6);
    case EACCES          : return fix(-7);
    case ELOOP           : return fix(-8);
    case EISDIR          : return fix(-9);
    case EROFS           : return fix(-10);
    case EMFILE          : return fix(-11);
    case ENFILE          : return fix(-12);
    case ENXIO           : return fix(-13);
    case EOPNOTSUPP      : return fix(-14);
    case ENOSPC          : return fix(-15);
    case EDQUOT          : return fix(-16);
    case EIO             : return fix(-17);
    case ETXTBSY         : return fix(-18);
    case EFAULT          : return fix(-19);
    case EEXIST          : return fix(-20);
    case EINVAL          : return fix(-21);
  }
  return fix(-1);
}


ikp
ikrt_close_fd(ikp fd, ikpcb* pcb){
  int err = close(unfix(fd));
  if(err == -1){
    return ikrt_io_error();
  } else {
    return false_object;;
  }
}

ikp
ikrt_open_input_fd(ikp fn, ikpcb* pcb){
  int fh = open((char*)(fn+off_bytevector_data), O_RDONLY, 0);
  if(fh > 0){
    return fix(fh);
  } else {
    return ikrt_io_error();
  }
}

ikp
ikrt_open_output_fd(ikp fn, ikp ikopts, ikpcb* pcb){
  int opts = unfix(ikopts);
  int mode = 0;
  switch (opts){
    /* mode 0: error if exists, create if does not exist */
    case 0: mode = O_WRONLY | O_CREAT | O_EXCL; break;
    /* mode 1: truncate if exists, error if not exists */
    case 1: mode = O_WRONLY | O_TRUNC; break;
    /* mode 2: truncate if exists, create if not exist */
    case 2: mode = O_WRONLY | O_TRUNC | O_CREAT ; break;
    /* mode 3: truncate if exists, error if not exists */
    case 3: mode = O_WRONLY | O_TRUNC ; break;
    case 4: mode = O_WRONLY | O_CREAT | O_EXCL ; break;
    case 5: mode = O_WRONLY | O_CREAT ; break;
    case 6: mode = O_WRONLY | O_CREAT ; break;
    case 7: mode = O_WRONLY ; break;
  }
  int fh = open((char*)(fn+off_bytevector_data), 
                mode,
                S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
  if(fh > 0){
    return fix(fh);
  } else {
    return ikrt_io_error();
  }
}



ikp
ikrt_read_fd(ikp fd, ikp bv, ikp off, ikp cnt, ikpcb* pcb){
  ssize_t bytes = 
   read(unfix(fd),
        (char*)(bv+off_bytevector_data+unfix(off)), 
        unfix(cnt));
  if(bytes >= 0){
    return fix(bytes);
  } else {
    return ikrt_io_error();
  }
}

ikp
ikrt_write_fd(ikp fd, ikp bv, ikp off, ikp cnt, ikpcb* pcb){
  ssize_t bytes = 
   write(unfix(fd),
         (char*)(bv+off_bytevector_data+unfix(off)), 
         unfix(cnt));
  if(bytes >= 0){
    return fix(bytes);
  } else {
    return ikrt_io_error();
  }
}

