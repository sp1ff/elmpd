dist_lisp_LISP = elmpd.el
SUBDIRS = test

dist-hook:
	$(EMACS) --batch --eval '(checkdoc-file "$(srcdir)/elmpd.el")'
