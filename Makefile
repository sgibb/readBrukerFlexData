## r binaries
R_BIN := R

PACKAGE_NAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PACKAGE_VERSION := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
SRC := $(shell basename $(PWD))
LOCALDIR := .local
TESTDIR := tests

.PHONY: clean check build install remove local_install local_remove test

all: clean check build

roxygen: 
	cd .. ;\
	$(R_BIN) -q -e "library(\"roxygen2\")" \
				      -e "roxygenize(\"$(PACKAGE_NAME)\")"

build: roxygen
	cd .. ;\
	$(R_BIN) CMD build $(SRC)

install: build
	cd .. ;\
	$(R_BIN) CMD INSTALL $(PACKAGE_NAME)_$(PACKAGE_VERSION).tar.gz
	
remove:
	$(R_BIN) CMD REMOVE $(PACKAGE_NAME)

local_install: local_remove 
	cd .. ;\
	mkdir $(LOCALDIR) ;\
	$(R_BIN) CMD INSTALL --library=$(LOCALDIR) $(PACKAGE_NAME)

local_remove:
	cd ..;\
	$(RM) -rf $(LOCALDIR)

test: local_install
	cd .. ;\
	$(R_BIN) -q -e "library(\"$(PACKAGE_NAME)\", lib.loc=\"$(LOCALDIR)\")" \
		   		    -e "library(\"testthat\")" \
				      -e "test_dir(\"$(TESTDIR)\")"

check: build
	cd .. ;\
	$(R_BIN) CMD check $(PACKAGE_NAME)_$(PACKAGE_VERSION).tar.gz --as-cran

clean: local_remove
	cd .. ;\
	$(RM) -rf $(PACKAGE_NAME).Rcheck/ \
	$(RM) -rf $(PACKAGE_NAME)_*.tar.gz

