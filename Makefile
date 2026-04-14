EMACS ?= emacs
ELPA_DIR := $(HOME)/.emacs.d/elpa

# Discover dependency directories (works with any version suffix)
LOAD_PATH := -L . \
	$(patsubst %,-L %,$(wildcard $(ELPA_DIR)/claude-code*)) \
	$(patsubst %,-L %,$(wildcard $(ELPA_DIR)/all-the-icons-*)) \
	$(patsubst %,-L %,$(wildcard $(ELPA_DIR)/nerd-icons-*))

PACKAGES := agents-workflow.el claude-dashboard.el codex-cli.el \
	databricks-runs.el jira-board.el slack-monitor.el github-prs.el

TEST_FILES := agents-workflow-tests.el claude-dashboard-tests.el \
	codex-cli-tests.el databricks-runs-tests.el slack-monitor-tests.el \
	github-prs-tests.el

.PHONY: all test compile checkdoc clean

all: checkdoc compile test

test:
	$(EMACS) --batch $(LOAD_PATH) -l ert \
	  $(addprefix -l ,$(TEST_FILES)) \
	  -f ert-run-tests-batch-and-exit

compile:
	$(EMACS) --batch $(LOAD_PATH) \
	  --eval "(setq byte-compile-error-on-warn t)" \
	  -f batch-byte-compile $(PACKAGES)

checkdoc:
	$(EMACS) --batch $(LOAD_PATH) \
	  --eval "(require 'checkdoc)" \
	  --eval "(setq sentence-end-double-space nil)" \
	  $(foreach f,$(PACKAGES),--eval "(checkdoc-file \"$(f)\")")

clean:
	rm -f *.elc
