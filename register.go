package goloader

import (
	"cmd/objfile/objfile"
	"encoding/binary"
	"os"
	"reflect"
	"strings"
	"unsafe"
)

const (
	TLSNAME        = "(TLS)"
	R_CALLIND_NAME = "R_CALLIND"
)

type interfaceHeader struct {
	typ  uintptr
	word unsafe.Pointer
}

type stringHeader struct {
	Data uintptr
	Len  int
}

type sliceHeader struct {
	Data uintptr
	Len  int
	Cap  int
}

//go:linkname block runtime.block
func block()

// RegSymbol register common types for relocation
func RegSymbol(symPtr map[string]uintptr) {

	var int0 = int(0)
	RegTypes(symPtr, int(0), int8(0), int16(0), int32(0),
		int64(0), uint(0), uint8(0), uint16(0), uint32(0), uint64(0), true, &int0,
		"", []byte{}, []uint{}, []int{}, uintptr(0), make(chan bool, 1), unsafe.Pointer(&symPtr))
	RegFunc(symPtr, "runtime.block", block)

	ex, err := os.Executable()
	mustOK(err)
	f, err := objfile.Open(ex)
	mustOK(err)
	defer f.Close()

	syms, err := f.Symbols()
	for _, sym := range syms {
		if sym.Code == 'T' && !strings.HasPrefix(sym.Name, "type..") {
			symPtr[sym.Name] = uintptr(sym.Addr)
		} else if strings.HasPrefix(sym.Name, "runtime.") {
			symPtr[sym.Name] = uintptr(sym.Addr)
		} else if strings.HasPrefix(sym.Name, "go.itab") {
			RegItab(symPtr, sym.Name, uintptr(sym.Addr))
		}
	}
}

func RegItab(symPtr map[string]uintptr, name string, addr uintptr) {
	symPtr[name] = uintptr(addr)
	bs := strings.TrimLeft(name, "go.itab.")
	bss := strings.Split(bs, ",")
	ptrs := *(*[2]uintptr)(unsafe.Pointer(addr))
	for i, ptr := range ptrs {
		typeName := bss[len(bss)-i-1]
		if typeName[0] == '*' {
			var obj interface{} = reflect.TypeOf(0)
			(*interfaceHeader)(unsafe.Pointer(&obj)).word = unsafe.Pointer(ptr)
			typ := obj.(reflect.Type).Elem()
			obj = typ
			typePtr := uintptr((*interfaceHeader)(unsafe.Pointer(&obj)).word)
			symPtr["type."+typeName[1:]] = typePtr
		}
		symPtr["type."+typeName] = ptr
	}
}

func RegTLS(symPtr map[string]uintptr, offset int) {
	var ptr interface{} = RegSymbol
	var ptr2 = *(*uintptr)((*interfaceHeader)(unsafe.Pointer(&ptr)).word)
	var ptr3 = (*[36]byte)(unsafe.Pointer(ptr2))
	var tlsValue = uintptr(binary.LittleEndian.Uint32(ptr3[offset:]))
	symPtr[TLSNAME] = tlsValue
}

func RegType(symPtr map[string]uintptr, name string, typ interface{}) {
	aHeader := (*interfaceHeader)(unsafe.Pointer(&typ))
	symPtr[name] = aHeader.typ
}

func RegFunc(symPtr map[string]uintptr, name string, f interface{}) {
	var ptr = *(*uintptr)((*interfaceHeader)(unsafe.Pointer(&f)).word)
	symPtr[name] = ptr
}

func getFuncPtr(f interface{}) uintptr {
	return *(*uintptr)((*interfaceHeader)(unsafe.Pointer(&f)).word)
}
