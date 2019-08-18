PROJECT_NAME=hs-compiler

TESTS_BASE=tests
IN_BASE=tests/input
OUT_BASE=tests/output

IN_EXT=.in
OUT_EXT=.out

TESTS_DIR=$(TESTS_BASE)
IN_DIR=$(addprefix $(IN_BASE), $(TESTS_DIR))

FILES=$(notdir $(basename $(wildcard $(IN_DIR)*$(IN_EXT))))
IN_FILES=$(addprefix $(IN_DIR), $(addsuffix $(IN_EXT), $(FILES)))
OUT_FILES=$(addprefix $(OUT_DIR), $(addsuffix $(OUT_EXT), $(FILES)))

IN_WILD=$(IN_DIR)%$(IN_EXT)
OUT_WILD=$(OUT_DIR)%$(OUT_EXT)

OUT_ALL=$(OUT_DIR)*$(OUT_EXT)

build:
	cabal new-build
	cp ./dist-newstyle/build/**/**/**/x/$(PROJECT_NAME)/build/$(PROJECT_NAME)/$(PROJECT_NAME) .