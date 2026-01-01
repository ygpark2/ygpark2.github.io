.PHONY: watch post post-map post-jungto vendor-js build rewatch deploy-local

watch:
	stack run ainsyl -- watch

post:
	@if [ -z "$(TITLE)" ] || [ -z "$(FILE)" ]; then \
		echo "Usage: make post TITLE=\"My Title\" FILE=\"my-post\""; \
		exit 1; \
	fi
	stack run post post -- --title "$(TITLE)" --file "$(FILE)"

post-map:
	@if [ -z "$(TITLE)" ] || [ -z "$(FILE)" ]; then \
		echo "Usage: make post-map TITLE=\"My Title\" FILE=\"my-post\""; \
		exit 1; \
	fi
	stack run post map -- --title "$(TITLE)" --file "$(FILE)"

post-jungto:
	@if [ -z "$(TITLE)" ] || [ -z "$(FILE)" ] || [ -z "$(THOUSAND)" ] || [ -z "$(HUNDRED)" ] || [ -z "$(DAY)" ]; then \
		echo "Usage: make post-jungto TITLE=\"Title\" FILE=\"file\" THOUSAND=10 HUNDRED=2 DAY=3"; \
		exit 1; \
	fi
	stack run post jungto -- --title "$(TITLE)" --file "$(FILE)" --thousand "$(THOUSAND)" --hundred "$(HUNDRED)" --day "$(DAY)"

vendor-js:
	npm install
	node scripts/vendor-js.js

build:
	bash scripts/incremental-build.sh

rewatch:
	stack build
	stack run ainsyl -- clean
	stack run ainsyl -- watch

deploy-local: BRANCH ?= src
deploy-local:
	@if [ -z "$(MSG)" ]; then \
		echo "Usage: make deploy-local MSG=\"Deploy message\""; \
		exit 1; \
	fi
	stack run ainsyl -- clean
	stack run ainsyl -- build
	DEPLOY_DIR=$$(mktemp -d); \
	git worktree add -B gh-pages $$DEPLOY_DIR origin/gh-pages; \
	rsync -a --delete _site/ $$DEPLOY_DIR/; \
	git -C $$DEPLOY_DIR add -A; \
	if [ -n "$$(git -C $$DEPLOY_DIR status --porcelain)" ]; then \
		git -C $$DEPLOY_DIR commit -m "$(MSG)"; \
	fi; \
	git -C $$DEPLOY_DIR push origin gh-pages; \
	git worktree remove $$DEPLOY_DIR
