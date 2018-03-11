
Goloader can load and run golang code at runtime.

## How it work?

Goloader works like linker, it relocates the address of symbols in object file, generates runnable code, reusing the runtime function and the type pointer of loader.

Goloader provide some information to the runtime and gc of go, making itself works correctly with them.

## Compare with plugin

Goloader reuses the runtime, much smaller and unloadable.

## Build

**Make sure you're using go 1.8, 1.9.**

First execute the following command, goloader relies on internal package, which is forbidden by go compiler.
```
cp -r $GOROOT/src/cmd/internal $GOROOT/src/cmd/objfile
```

## Example

```
go build github.com/dearplain/goloader/examples/loader

go tool compile $GOPATH/src/github.com/dearplain/goloader/examples/schedule/schedule.go
./loader -o schedule.o -run main.main

go tool compile $GOPATH/src/github.com/dearplain/goloader/examples/base/base.go
./loader -o base.o -run main.main

go tool compile $GOPATH/src/github.com/dearplain/goloader/examples/http/http.go
./loader -o http.o -run main.main
```

## Warning

Currently only tests and develops on golang 1.9 (x64/x86, darwin, linux, windows), be aware of golang's internal structs change, especially moduledata struct.
