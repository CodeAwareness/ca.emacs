.POSIX:

MAKEFLAGS += k
CASK = cask
EMACS ?= emacs

EMACSFLAGS       = -Q -batch -L .
COMPILE_COMMAND  = --eval "(setq byte-compile-error-on-warn t)" -f batch-byte-compile
CHECKDOC_COMMAND = -l "test/checkdock.el"
LINT_DIR         = /tmp/codeawareness
LINT_FLAG        = --eval "(setq byte-compile-dest-file-function (lambda (f) (concat \"$(LINT_DIR)\" (file-name-nondirectory f) \"c\")))"
TEST_COMMAND     = buttercup -L . $(NO_LOAD_WARNINGS)

ELS  = codeawareness.el codeawareness-config.el codeawareness-logger.el
ELCS = $(ELS:.el=.elc)

.PHONY: test compile checkdoc clean lint prepare clean-start .prepare-lint

.ONESHELL:

%.elc: %.el
	@printf "Compiling $<\n"
	$(CASK) exec $(EMACS) $(EMACSFLAGS) $(COMPILE_COMMAND) $<

compile: prepare $(ELCS)

.cask: Cask
	@echo Updating external dependencies...
	@$(CASK) install
	@$(CASK) update
	@touch .cask

prepare: .cask

test: prepare
	@$(CASK) exec $(TEST_COMMAND)

clean:
	@rm -f *.elc

lint: EMACSFLAGS += $(LINT_FLAG)
lint: .prepare-lint compile checkdoc
	@rm -rf $(LINT_DIR)

checkdoc:
	@$(CASK) exec $(EMACS) $(EMACSFLAGS) $(CHECKDOC_COMMAND)

clean-start: prepare
	@$(CASK) exec $(EMACS) -Q -L . --eval "(require 'codeawareness)" &

.prepare-lint:
	@rm -rf $(LINT_DIR)
	@mkdir -p $(LINT_DIR)
