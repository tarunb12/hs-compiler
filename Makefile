PROJECT_NAME=hs-compiler

build:
	cabal new-build
	cp ./dist-newstyle/build/**/**/**/x/$(PROJECT_NAME)/build/$(PROJECT_NAME)/$(PROJECT_NAME) .