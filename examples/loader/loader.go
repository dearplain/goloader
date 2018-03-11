package main

import (
	"cmd/objfile/goobj"
	"flag"
	"fmt"
	"net/http"
	"os"
	"reflect"
	"time"
	"unsafe"

	"github.com/dearplain/goloader"
	"github.com/kr/pretty"
)

func mustOK(err error) {
	if err != nil {
		panic(err)
	}
}

func main() {

	var file = flag.String("o", "", "load go object file")
	var pkgpath = flag.String("p", "main", "package path")
	var parseFile = flag.String("parse", "", "parse go object file")
	var run = flag.String("run", "main.main", "run function")

	flag.Parse()

	if *parseFile != "" {
		parse(parseFile, pkgpath)
		return
	}

	if *file == "" {
		flag.PrintDefaults()
		return
	}

	f, err := os.Open(*file)
	if err != nil {
		fmt.Println(err)
		return
	}
	defer f.Close()

	reloc, _ := goloader.ReadObj(f)
	symPtr := make(map[string]uintptr)
	goloader.RegSymbol(symPtr)

	goloader.RegTypes(symPtr, time.Duration(0))
	goloader.RegTypes(symPtr, reflect.ValueOf(0))
	// most of time you don't need to register function, but if loader complain about it, you have to.
	goloader.RegTypes(symPtr, http.ListenAndServe, http.Dir("/"),
		http.Handler(http.FileServer(http.Dir("/"))), http.FileServer, http.HandleFunc,
		&http.Request{})

	codeModule, err := goloader.Load(reloc, symPtr)
	if err != nil {
		fmt.Println("Load error:", err)
	}

	runFuncPtr := codeModule.Syms[*run]
	funcPtrContainer := (uintptr)(unsafe.Pointer(&runFuncPtr))
	runFunc := *(*func())(unsafe.Pointer(&funcPtrContainer))
	runFunc()

	codeModule.Unload()
}

func parse(file, pkgpath *string) {

	if *file == "" {
		flag.PrintDefaults()
		return
	}

	f, err := os.Open(*file)
	if err != nil {
		fmt.Printf("%# v\n", err)
		return
	}
	obj, err := goobj.Parse(f, *pkgpath)
	pretty.Printf("%# v\n", obj)
	f.Close()
	if err != nil {
		fmt.Printf("error reading %s: %v\n", *file, err)
		return
	}
}
