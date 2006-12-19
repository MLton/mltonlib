#include <sys/epoll.h>

enum EPOLL_CTL {
  CTL_ADD = EPOLL_CTL_ADD,
  CTL_DEL = EPOLL_CTL_DEL,
  CTL_MOD = EPOLL_CTL_MOD
};

int close(int);
