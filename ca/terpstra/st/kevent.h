#include <sys/event.h>
#include <sys/time.h>

enum filter { 
	read = EVFILT_READ,
	write = EVFILT_WRITE,
	aio = EVFILT_AIO,
	vnode = EVFILT_VNODE,
	proc = EVFILT_PROC,
	signal = EVFILT_SIGNAL,
	timer = EVFILT_TIMER,
	machport = EVFILT_MACHPORT,
	fs = EVFILT_FS
};

enum action {
	add = EV_ADD,
	delete = EV_DELETE,
	enable = EV_ENABLE,
	disable = EV_DISABLE,
	oneshot = EV_ONESHOT,
	clear = EV_CLEAR,
	sysflags = EV_SYSFLAGS,
	flag0 = EV_FLAG0,
	flag1 = EV_FLAG1,
	eof = EV_EOF,
	error = EV_ERROR,
	poll = EV_POLL,
	ooband = EV_OOBAND
};

/*
enum note {
	lowat = NOTE_LOWAT,
	delete = NOTE_DELETE,
	write = NOTE_WRITE,
	extend = NOTE_EXTEND,
	attrib = NOTE_ATTRIB,
	link = NOTE_LINK,
	rename = NOTE_RENAME,
	revoke = NOTE_REVOKE,
	exit = NOTE_EXIT,
	fork = NOTE_FORK,
	exec = NOTE_EXEC,
	pctrlmask = NOTE_PCTRLMASK,
	pdatamask = NOTE_PDATAMASK,
	seconds = NOTE_SECONDS,
	useconds = NOTE_USECONDS,
	nseconds = NOTE_NSECONDS,
	absolute = NOTE_ABSOLUTE,
	track = NOTE_TRACK,
	trackerr = NOTE_TRACKERR,
	child = NOTE_CHILD
};
*/

int close(int fd);
