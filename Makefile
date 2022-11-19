.PHONY: all format build test clean run app

all: format test

format:
	purs-tidy format-in-place src/**/*.purs test/**/*.purs

build:
	spago build

test:
	spago test

clean:
	rm -rf output .spago output-es .psci_modules

run:
	spago run

app:
	spago bundle-app
