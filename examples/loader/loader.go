package main

import (
	"cmd/objfile/goobj"
	"flag"
	"fmt"
	"net/http"
	"os"
	"reflect"
	"runtime"
	"sync"
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
	var times = flag.Int("times", 1, "run count")

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

	symPtr := make(map[string]uintptr)
	goloader.RegSymbol(symPtr)

	goloader.RegTypes(symPtr, time.Duration(0))
	goloader.RegTypes(symPtr, reflect.ValueOf(0))
	goloader.RegTypes(symPtr, runtime.LockOSThread)
	// most of time you don't need to register function, but if loader complain about it, you have to.
	goloader.RegTypes(symPtr, http.ListenAndServe, http.Dir("/"),
		http.Handler(http.FileServer(http.Dir("/"))), http.FileServer, http.HandleFunc,
		&http.Request{})
	w := sync.WaitGroup{}
	goloader.RegTypes(symPtr, w, w.Wait)

	reloc, _ := goloader.ReadObj(f)

	var mmapByte []byte
	for i := 0; i < *times; i++ {
		codeModule, err := goloader.Load(reloc, symPtr)
		if err != nil {
			fmt.Println("Load error:", err)
		}
		runFuncPtr := codeModule.Syms[*run]
		funcPtrContainer := (uintptr)(unsafe.Pointer(&runFuncPtr))
		runFunc := *(*func())(unsafe.Pointer(&funcPtrContainer))
		runFunc()
		codeModule.Unload()

		// a strict test, try to make mmap random
		if mmapByte == nil {
			mmapByte, err = goloader.Mmap(1024)
			if err != nil {
				fmt.Println(err)
			}
			b := make([]byte, 1024)
			copy(mmapByte, b) // reset all bytes
		} else {
			goloader.Munmap(mmapByte)
			mmapByte = nil
		}
	}

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
