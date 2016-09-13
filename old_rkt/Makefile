# speed-dial
# See LICENSE file for copyright and license details.

include config.mk

SRC = speed-dial.rkt
VPATH = .:racket_generic:modules

all: options speed-dial

options:
	@echo speed-dial build options:
	@echo "RACO       = ${RACO}"

speed-dial: 
	@echo raco exe speed-dial.rkt
	@${RACO} $@.rkt

clean:
	@echo cleaning...
	@rm -fv speed-dial speed-dial-${VERSION}.tar.gz

#dist: clean
#	@echo creating dist tarball
#	@mkdir -p speed-dial-${VERSION}
#	@cp -R LICENSE.txt Makefile README.adoc config.def.h config.mk \
#		speed-dial.1 speed-dial.h ${SRC} speed-dial-${VERSION}
#	@tar -cf speed-dial-${VERSION}.tar speed-dial-${VERSION}
#	@gzip speed-dial-${VERSION}.tar
#	@rm -rf speed-dial-${VERSION}

install: all
	@echo installing executable file to ${DESTDIR}${PREFIX}/bin
	@mkdir -p ${DESTDIR}${PREFIX}/bin
	@cp -f speed-dial ${DESTDIR}${PREFIX}/bin
	@chmod 755 ${DESTDIR}${PREFIX}/bin/speed-dial
	@echo installing speed-dial.conf to ${DESTDIR}${SHARE}/speed-dial/
	@mkdir -p ${DESTDIR}${SHARE}/speed-dial
	@cp -rfv speed-dial.conf ${DESTDIR}${SHARE}/speed-dial/speed-dial.conf
	@chmod 644 ${DESTDIR}${SHARE}/speed-dial/*
	@echo installing manual page to ${DESTDIR}${MANPREFIX}/man1
	@mkdir -p ${DESTDIR}${MANPREFIX}/man1
	@sed "s/VERSION/${VERSION}/g" < speed-dial.1 > ${DESTDIR}${MANPREFIX}/man1/speed-dial.1
	@chmod 644 ${DESTDIR}${MANPREFIX}/man1/speed-dial.1

uninstall:
	@echo removing executable file from ${DESTDIR}${PREFIX}/bin
	@rm -f ${DESTDIR}${PREFIX}/bin/speed-dial
	@echo removing data in /usr/local/share from ${DESTDIR}${SHARE}/speed-dial
	@rm -rf ${DESTDIR}${SHARE}/speed-dial
	@echo removing manual page from ${DESTDIR}${MANPREFIX}/man1
	@rm -f ${DESTDIR}${MANPREFIX}/man1/speed-dial.1

.PHONY: all options clean dist install uninstall
