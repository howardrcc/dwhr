# dwhr-runtime — thin wrapper over docker compose.
# Spec: docs/DOCKER.md.
#
# Usage:
#   make build                            # build the dwhr-runtime image
#   make demo                             # run 15PdfShowcase on :4815
#   make demo EXAMPLE=16D3Sankey          # run a different example
#   make demo EXAMPLE=01SimpleTable PORT=4900
#   make shell                            # interactive bash inside the runtime
#   make check                            # devtools::check against /workspaces/dwhr
#   make stop                             # stop the running compose stack
#   make clean                            # tear down volumes + remove image

EXAMPLE ?= 15PdfShowcase
PORT    ?= 4815

export EXAMPLE
export PORT

.PHONY: build demo shell check stop clean help

help:
	@awk 'BEGIN{FS=":.*?##"} /^[a-zA-Z0-9_-]+:.*?##/ {printf "  %-12s %s\n", $$1, $$2}' $(MAKEFILE_LIST)

build: ## Build the dwhr-runtime image (~15 min cold, <1 min cached)
	docker buildx build --tag dwhr-runtime:0.1.0 --tag dwhr-runtime:latest --load .

demo: ## Run an example (EXAMPLE=15PdfShowcase, PORT=4815)
	@echo "→ http://localhost:$(PORT)  (example: $(EXAMPLE))"
	docker compose up

shell: ## Drop into bash inside the runtime container
	docker compose run --rm --service-ports runtime bash

check: ## R CMD check dwhr inside the container (no host R required)
	docker compose run --rm runtime \
	    Rscript -e 'devtools::check("/workspaces/dwhr", error_on = "warning")'

stop: ## Stop the running compose stack
	docker compose down

clean: ## Tear down volumes + remove the image
	docker compose down -v
	-docker image rm dwhr-runtime:0.1.0 dwhr-runtime:latest
