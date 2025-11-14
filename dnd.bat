@echo off
cargo run -- demos\%1.tpas -a cil --debug --print-stage=ir > target\%1.txt
cargo run -- demos\%1.tpas -a cil --debug -o target\%1.lib
cargo run -- demos\%1.tpas -a cil --debug -o target\%1.dll
dotnet target\%1.dll