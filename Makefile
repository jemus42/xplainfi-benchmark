# Environment setup for the xplainfi benchmark.
#
# Three backends, installed independently:
#   - R packages        via rv   (rv.lock)
#   - Python venv        via uv   (uv.lock)   -> fippy / sage reference impls
#   - libtorch           via torch::install_torch()  -> mlr3torch (mlp/boosting)
#
# `make setup` does all three; `make check` reports what's missing without
# installing anything (run it before submitting a batch).

.PHONY: help setup r-deps py-deps torch check

help: ## Show available targets
	@grep -E '^[a-z-]+:.*##' $(MAKEFILE_LIST) | \
	  awk 'BEGIN{FS=":.*## "}{printf "  \033[1m%-9s\033[0m %s\n", $$1, $$2}'

setup: r-deps py-deps torch ## Install everything (R + Python + libtorch)

r-deps: ## Sync the R library to rv.lock
	rv sync

py-deps: ## Sync the Python venv to uv.lock
	uv sync --frozen

torch: ## Download libtorch for mlr3torch if not already installed
	rv run -e 'if (torch::torch_is_installed()) message("libtorch already installed") else torch::install_torch()'

check: ## Report sync status of all three backends (no install)
	@echo "== R packages (rv) =="
	@rv plan --locked >/dev/null 2>&1 && echo "  OK   library matches rv.lock" || echo "  TODO run 'make r-deps'"
	@echo "== Python venv (uv) =="
	@uv sync --check >/dev/null 2>&1 && echo "  OK   .venv matches uv.lock" || echo "  TODO run 'make py-deps'"
	@echo "== libtorch (mlr3torch) =="
	@rv run --no-sync -e 'q(status = as.integer(!torch::torch_is_installed()))' >/dev/null 2>&1 && echo "  OK   libtorch installed" || echo "  TODO run 'make torch'"
