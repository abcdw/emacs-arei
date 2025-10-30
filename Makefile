GUIXTM=guix time-machine -C ./env/guix/arei/env/guix/channels.scm
EMACS=$(GUIXTM) -- shell emacs emacs-ox-html-stable-ids -- emacs
HUT=$(GUIXTM) -- shell hut -- hut

README.html: README
	${EMACS} -Q --batch -l docs/html-export-config.el README \
	--funcall org-html-export-to-html

deploy-README.html: README.html
	${HUT} git update --readme README.html \
	--repo https://git.sr.ht/~abcdw/emacs-arei
