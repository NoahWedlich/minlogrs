default: all

all: build build-debug doc test bench lint format

.PHONY: build build-debug doc test bench lint format

build:
	cargo build --release --all-targets --all-features
build-debug:
	cargo build --all-targets --all-features
doc:
	cargo doc
test:
	cargo test
bench:
	cargo bench
lint:
	cargo clippy --all-targets --all-features
format:
	cargo fmt
