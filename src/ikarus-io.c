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


#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/uio.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netdb.h>
#include <string.h>
#include <netinet/in.h>
#include "ikarus-data.h"

ikptr 
ikrt_io_error(){
  int err = errno;
#if 0
  fprintf(stderr, "errno=%d %s\n", err, strerror(err));
#endif
  switch(err){
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
    case EAGAIN          : return fix(-22); /* hardcoded in ikarus.io.ss */
    case EPIPE           : return fix(-23);
    case ECONNREFUSED    : return fix(-24);
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
  int fh = open((char*)(long)(fn+off_bytevector_data), O_RDONLY, 0);
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
  int fh = open((char*)(long)(fn+off_bytevector_data), 
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
#if 0
  fprintf(stderr, "READ: %d\n", unfix(fd));
#endif
  ssize_t bytes = 
   read(unfix(fd),
        (char*)(long)(bv+off_bytevector_data+unfix(off)), 
        unfix(cnt));
#if 0
  fprintf(stderr, "BYTES: %d\n", bytes);
#endif
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
         (char*)(long)(bv+off_bytevector_data+unfix(off)), 
         unfix(cnt));
  if(bytes >= 0){
    return fix(bytes);
  } else {
    return ikrt_io_error();
  }
}



static ikptr
do_connect(ikptr host, ikptr srvc, int socket_type){
  struct addrinfo* info;
  int err = getaddrinfo((char*)(long)(host+off_bytevector_data),
                        (char*)(long)(srvc+off_bytevector_data),
                        0,
                        &info);
  if(err){
    return fix(-1);
  }
  struct addrinfo* i = info;
  ikptr sock = fix(-1);
  while(i){
    if(i->ai_socktype != socket_type){
      i = i->ai_next;
    } else {
      int s = socket(i->ai_family, i->ai_socktype, i->ai_protocol);
      if(s < 0){
        sock = ikrt_io_error();
        i = i->ai_next;
      } else {
        int err = connect(s, i->ai_addr, i->ai_addrlen);
        if(err < 0){
          sock = ikrt_io_error();
          i = i->ai_next;
        } else {
          sock = fix(s);
          i = 0;
        }
      }
    }
  }
  freeaddrinfo(info);
  return sock;
}

ikptr
ikrt_tcp_connect(ikptr host, ikptr srvc, ikpcb* pcb){
  return do_connect(host, srvc, SOCK_STREAM);
}

ikptr
ikrt_udp_connect(ikptr host, ikptr srvc, ikpcb* pcb){
  return do_connect(host, srvc, SOCK_DGRAM);
}

ikptr 
ikrt_make_fd_nonblocking(ikptr fdptr, ikpcb* pcb){
  int fd = unfix(fdptr);
  int err = fcntl(fd, F_SETFL, O_NONBLOCK);
  if(err == -1){
    return ikrt_io_error();
  }
  return 0;
}

ikptr 
ikrt_select(ikptr fds, ikptr rfds, ikptr wfds, ikptr xfds, ikpcb* pcb){
  int rv = select(unfix(fds),
                  (fd_set*)(rfds + off_bytevector_data),
                  (fd_set*)(wfds + off_bytevector_data),
                  (fd_set*)(xfds + off_bytevector_data),
                  NULL);
  if(rv < 0){
    return ikrt_io_error();
  } 
  return fix(rv);
}

ikptr
ikrt_listen(ikptr port, ikpcb* pcb){
  
  int sock = socket(AF_INET, SOCK_STREAM, 0);
  if(sock < 0){
    return ikrt_io_error();
  }

  struct sockaddr_in servaddr;
  memset(&servaddr, 0, sizeof(struct sockaddr_in));
  servaddr.sin_family = AF_INET;
  servaddr.sin_addr.s_addr = htonl(INADDR_ANY);
  servaddr.sin_port = htons(unfix(port));

  int err;

  int reuse = 1;
  err = setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, &reuse, sizeof(int));
  if(err < 0){
    return ikrt_io_error();
  }


  err = bind(sock, (struct sockaddr *)&servaddr, sizeof(servaddr));
  if(err < 0){
    return ikrt_io_error();
  }

  err = listen(sock, 1024);
  if(err < 0){
    return ikrt_io_error();
  }
  return fix(sock);
}

ikptr
ikrt_accept(ikptr s, ikpcb* pcb){
  int sock = accept(unfix(s), NULL, NULL);
  if(sock < 0){
    return ikrt_io_error();
  } 
  return fix(sock);
}

ikptr
ikrt_shutdown(ikptr s, ikpcb* pcb){
#ifdef __CYGWIN__
  int err = close(unfix(s));
#else
  int err = shutdown(unfix(s), SHUT_RDWR);
#endif
  if(err < 0){
    return ikrt_io_error();
  } 
  return 0;
}


ikptr
ikrt_file_ctime(ikptr filename, ikptr res){
  struct stat s;
  int err = stat((char*)(filename + off_bytevector_data), &s);
  if(err) {
    return fix(errno);
  }
  
  ref(res, off_car) = fix(s.st_ctime);
  ref(res, off_cdr) = 0;
  return fix(0);
}

