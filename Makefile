.PHONY: all clean build serve

all: build

# Build the project
build:
	dune build
	cp _build/default/src/h42n42.bc.js static/h42n42.js

# Clean build artifacts
clean:
	dune clean
	rm -f static/h42n42.js

# Start a simple HTTP server to serve the game
serve: build
	cd static && python3 -m http.server 8080