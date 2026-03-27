EMACS ?= emacs
ELPA_DIR := $(HOME)/.emacs.d/elpa
ALL_ICONS_DIR := $(wildcard $(ELPA_DIR)/all-the-icons-*)
NERD_ICONS_DIR := $(wildcard $(ELPA_DIR)/nerd-icons-*)
LOAD_PATH := -L . \
	-L $(ELPA_DIR)/claude-code \
	-L $(ALL_ICONS_DIR) \
	-L $(NERD_ICONS_DIR)

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
