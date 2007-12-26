#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/uio.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netdb.h>

#include "ikarus-data.h"

ikptr 
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


ikptr
ikrt_close_fd(ikptr fd, ikpcb* pcb){
  int err = close(unfix(fd));
  if(err == -1){
    return ikrt_io_error();
  } else {
    return false_object;;
  }
}

ikptr
ikrt_open_input_fd(ikptr fn, ikpcb* pcb){
  int fh = open((char*)(fn+off_bytevector_data), O_RDONLY, 0);
  if(fh > 0){
    return fix(fh);
  } else {
    return ikrt_io_error();
  }
}

ikptr
ikrt_open_output_fd(ikptr fn, ikptr ikopts, ikpcb* pcb){
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



ikptr
ikrt_read_fd(ikptr fd, ikptr bv, ikptr off, ikptr cnt, ikpcb* pcb){
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

ikptr
ikrt_write_fd(ikptr fd, ikptr bv, ikptr off, ikptr cnt, ikpcb* pcb){
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

char* get_family(int x){
  if (x == AF_UNIX) return "AF_UNIX";
  if (x == AF_INET) return "AF_INET";
  if (x == AF_ISO) return "AF_ISO";
  if (x == AF_NS) return "AF_NS";
  if (x == AF_IMPLINK) return "AF_IMPLINK";
  return "AF_UNKNOWN";
}

char* get_type(int x){
  if (x == SOCK_STREAM) return "SOCK_STREAM";
  if (x == SOCK_DGRAM) return "SOCK_DGRAM";
  if (x == SOCK_RAW) return "SOCK_RAW";
  if (x == SOCK_SEQPACKET) return "SOCK_SEQPACKET";
  if (x == SOCK_RDM) return "SOCK_RDM";
  return "SOCK_UNKNOWN";
}

ikptr
ikrt_tcp_connect(ikptr host, ikptr srvc, ikpcb* pcb){
  struct addrinfo* info;
  int err = getaddrinfo(host+off_bytevector_data,
                        srvc+off_bytevector_data,
                        0,
                        &info);
  if(err){
    return fix(-1);
  }
  struct addrinfo* i = info;
  int sock = -1;
  while(i){
    if(i->ai_socktype != SOCK_STREAM){
      i = i->ai_next;
    } else {
      int s = socket(i->ai_family, i->ai_socktype, i->ai_protocol);
      if(s < 0){
        i = i->ai_next;
      } else {
        int err = connect(s, i->ai_addr, i->ai_addrlen);
        if(err < 0){
          i = i->ai_next;
        } else {
          sock = s;
          i = 0;
        }
      }
    }
  }
  freeaddrinfo(info);
  return fix(sock);
}

//ikptr
//ikrt_tcp_connect(ikp host, ikp port, ikpcb* pcb){
//
//}


