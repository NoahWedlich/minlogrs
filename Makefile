default: all

all: build build-debug doc lint

.PHONY: build build-debug doc lint

build:
	cargo build --release --all-targets --all-features
build-debug:
	cargo build --all-targets --all-features
doc:
	cargo doc
lint:
	cargo clippy --all-targets --all-features