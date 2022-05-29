default:
	just -l

emu BIN *ARGS:
    cargo run -p dlx --features cli --bin emu --quiet -- {{ ARGS }} {{ BIN }}

viz BIN *ARGS:
    cargo run -p dlx --features cli --bin viz --quiet -- {{ ARGS }} {{ BIN }}

build SRC *ARGS:
	cargo run -p tinyc -- {{ ARGS }} {{ SRC }}

e2e SRC BIN *ARGS:
	just build {{ SRC }} --arch dlx -o {{ BIN }} {{ ARGS }}
	just emu {{ BIN }}

file-bug CONTENT:
	echo "- [ ] {{ CONTENT }}" >> ISSUES.md

check:
    cargo clippy --no-deps -- -D warnings

consume BRANCH:
    git checkout master && git pull origin master && git branch -D {{ BRANCH }}
