all:	st
	
epoll/epoll.mlb:	epoll.h /usr/include/x86_64-linux/i386-linux/sys/epoll.h 
kevent/kevent.mlb:	kevent.h /usr/include/sys/event.h

%.mlb:
	mlnlffigen -allSU true -linkage static -dir $(@D) -mlbfile $(@F) $^

-include $(patsubst %.mlb,%.dep,$(wildcard *.mlb))
