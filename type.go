package goloader

import (
	"reflect"
	"runtime"
	"strings"
	"unsafe"
)

type tflag uint8

type _type struct {
	size       uintptr
	ptrdata    uintptr // size of memory prefix holding all pointers
	hash       uint32
	tflag      tflag
	align      uint8
	fieldalign uint8
	kind       uint8
	alg        *typeAlg
	// gcdata stores the GC type data for the garbage collector.
	// If the KindGCProg bit is set in kind, gcdata is a GC program.
	// Otherwise it is a ptrmask bitmap. See mbitmap.go for details.
	gcdata    *byte
	str       nameOff
	ptrToThis typeOff
}

type typeAlg struct {
	// function for hashing objects of this type
	// (ptr to object, seed) -> hash
	hash func(unsafe.Pointer, uintptr) uintptr
	// function for comparing objects of this type
	// (ptr to object A, ptr to object B) -> ==?
	equal func(unsafe.Pointer, unsafe.Pointer) bool
}

type imethod struct {
	name nameOff
	ityp typeOff
}

type interfacetype struct {
	typ     _type
	pkgpath name
	mhdr    []imethod
}

type uncommonType struct {
	pkgPath nameOff // import path; empty for built-in types like int, string
	mcount  uint16  // number of methods
	_       uint16  // unused
	moff    uint32  // offset from this uncommontype to [mcount]method
	_       uint32  // unused
}

type name struct {
	bytes *byte
}

//go:linkname (*_type).uncommon runtime.(*_type).uncommon
func (t *_type) uncommon() *uncommonType

//go:linkname (*_type).nameOff runtime.(*_type).nameOff
func (t *_type) nameOff(off nameOff) name

//go:linkname (*_type).typeOff runtime.(*_type).typeOff
func (t *_type) typeOff(off typeOff) *_type

//go:linkname name.name runtime.name.name
func (n name) name() (s string)

func (t *_type) PkgPath() string {
	ut := t.uncommon()
	if ut == nil {
		return ""
	}
	return t.nameOff(ut.pkgPath).name()
}

func (t *_type) Name() string {
	return t.nameOff(t.str).name()
}

func (t *_type) Type() reflect.Type {
	var obj interface{} = reflect.TypeOf(0)
	(*interfaceHeader)(unsafe.Pointer(&obj)).word = unsafe.Pointer(t)
	typ := obj.(reflect.Type)
	return typ
}

func ToType(typ reflect.Type) *_type {
	var obj interface{} = typ
	typePtr := uintptr((*interfaceHeader)(unsafe.Pointer(&obj)).word)
	return (*_type)(unsafe.Pointer(typePtr))
}

func GetFunctionName(i interface{}) string {
	return runtime.FuncForPC(reflect.ValueOf(i).Pointer()).Name()
}

func RegTypes(symPtr map[string]uintptr, interfaces ...interface{}) {
	for _, ins := range interfaces {
		v := reflect.ValueOf(ins)
		regTypeInfo(symPtr, v)
		if v.Kind() == reflect.Ptr {
			v = v.Elem()
			regTypeInfo(symPtr, v)
		}
	}
}

func regTypeInfo(symPtr map[string]uintptr, v reflect.Value) {
	ins := v.Interface()
	header := (*interfaceHeader)(unsafe.Pointer(&ins))

	var ptr uintptr
	var typePrefix string
	var symName string
	if v.Kind() == reflect.Func {
		ptr = *(*uintptr)(unsafe.Pointer(header.word))
		symName = GetFunctionName(ins)
	} else {
		ptr = uintptr(header.typ)
		typePrefix = "type."
		symName = v.Type().String()
	}

	if symName[0] == '*' {
		typePrefix += "*"
		symName = symName[1:]
	}

	pkgPath := (*_type)(unsafe.Pointer(header.typ)).PkgPath()

	var symFullName string
	lastSlash := strings.LastIndexByte(pkgPath, '/')
	if lastSlash > -1 {
		symFullName = typePrefix + pkgPath[:lastSlash] + "/" + symName
	} else {
		symFullName = typePrefix + symName
	}
	symPtr[symFullName] = ptr
	// fmt.Println(symFullName, v.Kind())
}

//go:linkname getitab runtime.getitab
func getitab(inter int, typ int, canfail bool) int

func addIFaceSubFuncType(funcTypeMap map[string]*int, typemap map[typeOff]uintptr,
	inter *interfacetype, dataBase int) {
	pkgPath := inter.typ.PkgPath()
	lastSlash := strings.LastIndexByte(pkgPath, '/')
	var head = pkgPath
	if lastSlash > -1 {
		head = pkgPath[lastSlash+1:]
	}
	ni := len(inter.mhdr)
	for k := 0; k < ni; k++ {
		i := &inter.mhdr[k]
		itype := inter.typ.typeOff(i.ityp)
		name := itype.Name()
		if name[0] == '*' {
			name = name[1:]
		}
		name = strings.Replace(name, head+".", pkgPath+".", -1)
		name = "type." + name
		if symAddrPtr, ok := funcTypeMap[name]; ok {
			itypePtr := int(uintptr(unsafe.Pointer(itype)))
			*symAddrPtr = itypePtr
			// fmt.Println(*symAddrPtr)
			typemap[typeOff(itypePtr-dataBase)] = uintptr(itypePtr)
			// fmt.Println(name, itypePtr-dataBase, itypePtr)
		}
	}
}
