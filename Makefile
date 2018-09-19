emacs ?= emacs
cask ?= cask

.PHONY: test

test:
	$(cask) exec ert-runner --reporter ert
