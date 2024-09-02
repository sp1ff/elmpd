dist_lisp_LISP = elmpd.el
EXTRA_DIST = README.org
CLEANFILES = .pkg-tmp elmpd-$(PACKAGE_VERSION).tar
SUBDIRS = doc test
AM_ELCFLAGS = --eval '(require (quote bytecomp))'

package: elmpd-$(PACKAGE_VERSION).tar $(srcdir)/README.org

srclisp=$(dist_lisp_LISP:%.el=$(srcdir)/%.el)

elmpd-$(PACKAGE_VERSION).tar: $(srclisp) $(srcdir)/README.org
	mkdir -p .pkg-tmp/elmpd-$(PACKAGE_VERSION)/ && \
	cp $(srclisp) .pkg-tmp/elmpd-$(PACKAGE_VERSION)/ && \
	cp $(srcdir)/README.org .pkg-tmp/elmpd-$(PACKAGE_VERSION)/ && \
	cd .pkg-tmp && tar cf $@ elmpd-$(PACKAGE_VERSION)/ && \
	cd .. && mv -v .pkg-tmp/elmpd-$(PACKAGE_VERSION).tar . && \
	rm -rf .pkg-tmp

dist-hook: package
