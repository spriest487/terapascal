set -e
cargo run --features backend-c -- demos/$1.tpas --debug --dump=ir > target/$1.txt
cargo run --features backend-c -- demos/$1.tpas --debug -o target/$1
target/$1
