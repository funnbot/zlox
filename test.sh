zig build
[ $? -eq 0 ] || exit 1
./zig-cache/bin/zlox test.lox