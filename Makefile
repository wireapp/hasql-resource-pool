PROJECT_NAME            := hasql-resource-pool

# https://www.gnu.org/software/make/manual/html_node/Special-Variables.html
# https://ftp.gnu.org/old-gnu/Manuals/make-3.80/html_node/make_17.html
PROJECT_MKFILE_PATH     := $(word $(words $(MAKEFILE_LIST)),$(MAKEFILE_LIST))
PROJECT_MKFILE_DIR      := $(shell cd $(shell dirname $(PROJECT_MKFILE_PATH)); pwd)

PROJECT_ROOT            := $(PROJECT_MKFILE_DIR)
LOCAL_UNTRACK_DIR       := $(PROJECT_MKFILE_DIR)/.local
CABAL_BUILD_DIR			:= $(CABAL_DIR)
DISTRIBUTIONS           := $(PROJECT_ROOT)/dist-newstyle/sdist
BACKEND_CABAL_CMD       := CABAL_BUILDDIR=$(CABAL_BUILD_DIR) cabal --project-dir=$(PROJECT_ROOT)


$(PROJECT_ROOT)/cabal.project.local:
	$(MAKE) update-local


.PHONY: update-local
update-local:
	$(BACKEND_CABAL_CMD) v2-update
	$(BACKEND_CABAL_CMD) v2-configure


.PHONY: build-local
build-local:	$(PROJECT_ROOT)/library					\
				$(PROJECT_ROOT)/$(PROJECT_NAME).cabal	\
				$(PROJECT_ROOT)/cabal.project.local
	$(BACKEND_CABAL_CMD) v2-build all


.PHONY: repl
repl:	build-local
	$(BACKEND_CABAL_CMD) v2-repl $(PROJECT_NAME)


.PHONY: run
run:
	$(BACKEND_CABAL_CMD) v2-run -- --help


.PHONY: test
test:
	$(BACKEND_CABAL_CMD) v2-test all


.PHONY: run-example
run-example:
	echo "done"


.PHONY: distribute
distribute: build-local
	$(BACKEND_CABAL_CMD) v2-sdist


.PHONY: publish
publish: | cleanall distribute
	cabal upload $(DISTRIBUTIONS)/$(PROJECT_NAME)-*.tar.gz


.PHONY: release
release:
	cabal upload $(DISTRIBUTIONS)/$(PROJECT_NAME)-*.tar.gz --publish


.PHONY: cleanall
cleanall:
	rm -rvf $(DISTRIBUTIONS)
