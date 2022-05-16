default:
	just -l

emu BINARY:
    cargo run -p dlx --features emu-cli --bin emu -- {{ BINARY }}

build SRC *ARGS:
	cargo run -p tinyc -- {{ ARGS }} {{ SRC }}

e2e SRC BIN *ARGS:
	just build {{ SRC }} --arch dlx -o {{ BIN }} {{ ARGS }}
	just emu {{ BIN }}

file-bug CONTENT:
	echo "- [ ] {{ CONTENT }}" >> ISSUES.md
