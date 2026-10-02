.DEFAULT_GOAL := help

CABAL ?= cabal
CABAL_FLAGS ?= --flags=threaded
BUILD_DIR ?= dist-newstyle
PYTHON ?= python3
TIMEOUT ?= 300
ISERV_PROXY ?= $(shell $(CABAL) list-bin --builddir="$(BUILD_DIR)" $(CABAL_FLAGS) exe:iserv-proxy)
CHILD_PYTHON ?= $(PYTHON)

.PHONY: help build check test verify

##@ Building
build: ## Build the proxy (CABAL_FLAGS, BUILD_DIR, TIMEOUT)
	timeout -k 5 $(TIMEOUT) $(CABAL) build --builddir="$(BUILD_DIR)" $(CABAL_FLAGS) exe:iserv-proxy

##@ Verification
check: ## Check whitespace and Python fixture syntax
	git diff --check
	$(PYTHON) -m py_compile tests/lifecycle.py tests/fake-interpreter.py

test: ## Test a built POSIX proxy (ISERV_PROXY, CHILD_PYTHON, TIMEOUT)
	@test -x "$(ISERV_PROXY)" || { echo 'Build the proxy or set ISERV_PROXY to its executable.' >&2; exit 1; }
	timeout -k 5 $(TIMEOUT) $(PYTHON) tests/lifecycle.py --proxy "$(ISERV_PROXY)" --child-python "$(CHILD_PYTHON)"

verify: check build test ## Build and test the proxy

##@ Help
help: ## Show targets and their parameters
	@awk 'BEGIN { FS = ":.*##" } /^##@/ { printf "\n%s\n", substr($$0, 5) } /^[a-zA-Z0-9_-]+:.*##/ { printf "  %-12s %s\n", $$1, $$2 }' $(MAKEFILE_LIST)
