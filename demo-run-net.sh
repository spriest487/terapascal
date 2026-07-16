set -e

dotnet build terapascal-net/terapascal2cil
cargo run --features backend-cil -- demos/$1.tpas -a cil --debug --dump=ir > target/$1.txt
cargo run --features backend-cil -- demos/$1.tpas -a cil --debug -o target/$1.lib

cargo run -p terapascal-demo-runner -- demos --filter=demos/$1.tpas --exec=dotnet --compiler=target/debug/terapascal --debug --target=target --verbose