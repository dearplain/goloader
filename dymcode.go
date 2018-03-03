package goloader

import (
	"bytes"
	"cmd/objfile/goobj"
	"encoding/binary"
	"errors"
	"fmt"
	"io"
	"runtime"
	"strconv"
	"strings"
	"sync"
	"unsafe"
)

func mustOK(err error) {
	if err != nil {
		panic(err)
	}
}

// copy from $GOROOT/src/cmd/internal/objabi/reloctype.go
const (
	// R_TLS_LE, used on 386, amd64, and ARM, resolves to the offset of the
	// thread-local symbol from the thread local base and is used to implement the
	// "local exec" model for tls access (r.Sym is not set on intel platforms but is
	// set to a TLS symbol -- runtime.tlsg -- in the linker when externally linking).
	R_TLS_LE    = 16
	R_CALL      = 8
	R_CALLARM   = 9
	R_CALLARM64 = 10
	R_CALLIND   = 11
	R_PCREL     = 15
	R_ADDR      = 1
	// R_ADDROFF resolves to a 32-bit offset from the beginning of the section
	// holding the data being relocated to the referenced symbol.
	R_ADDROFF = 5
	// R_WEAKADDROFF resolves just like R_ADDROFF but is a weak relocation.
	// A weak relocation does not make the symbol it refers to reachable,
	// and is only honored by the linker if the symbol is in some other way
	// reachable.
	R_WEAKADDROFF = 6
	// R_METHODOFF resolves to a 32-bit offset from the beginning of the section
	// holding the data being relocated to the referenced symbol.
	// It is a variant of R_ADDROFF used when linking from the uncommonType of a
	// *rtype, and may be set to zero by the linker if it determines the method
	// text is unreachable by the linked program.
	R_METHODOFF = 24
)

// copy from $GOROOT/src/cmd/internal/objabi/symkind.go
const (
	// An otherwise invalid zero value for the type
	Sxxx = iota
	// Executable instructions
	STEXT
	// Read only static data
	SRODATA
	// Static data that does not contain any pointers
	SNOPTRDATA
	// Static data
	SDATA
	// Statically data that is initially all 0s
	SBSS
	// Statically data that is initially all 0s and does not contain pointers
	SNOPTRBSS
	// Thread-local data that is initally all 0s
	STLSBSS
	// Debugging data
	SDWARFINFO
	SDWARFRANGE
)

type SymData struct {
	Name   string
	Kind   int
	Offset int
	Reloc  []Reloc
}

type Reloc struct {
	Offset int
	SymOff int
	Size   int
	Type   int
	Add    int
}

type CodeReloc struct {
	Code []byte
	Data []byte
	Mod  Module
	Syms []SymData
}

type CodeModule struct {
	Syms       map[string]uintptr
	CodeByte   []byte
	Module     *moduledata
	pcfuncdata []findfuncbucket
	stkmaps    [][]byte
}

var (
	fmodule     *moduledata
	tmpModule   *moduledata
	modulesLock sync.Mutex
)

func ReadObj(f io.ReadSeeker) (*CodeReloc, error) {
	obj, err := goobj.Parse(f, "main")
	if err != nil {
		return nil, fmt.Errorf("read error: %v", err)
	}

	var syms = make(map[string]*goobj.Sym)
	for _, sym := range obj.Syms {
		syms[sym.Name] = sym
	}

	var symMap = make(map[string]int)
	var gcObjs = make(map[string]uintptr)
	var fileTabOffsetMap = make(map[string]int)

	var reloc CodeReloc

	for _, sym := range obj.Syms {
		if sym.Kind == STEXT {
			relocSym(&reloc, f, sym, syms, symMap,
				gcObjs, fileTabOffsetMap)
		}
	}

	return &reloc, nil
}

func addSym(symMap map[string]int, symArray *[]SymData, rsym *SymData) int {
	var offset int
	if of, ok := symMap[rsym.Name]; !ok {
		offset = len(*symArray)
		*symArray = append(*symArray, *rsym)
		symMap[rsym.Name] = offset
	} else {
		offset = of
		(*symArray)[offset] = *rsym
	}
	return offset
}

type readAtSeeker struct {
	io.ReadSeeker
}

func (r *readAtSeeker) ReadAt(p []byte, offset int64) (n int, err error) {
	_, err = r.Seek(offset, io.SeekStart)
	if err != nil {
		return
	}
	return r.Read(p)
}

func relocSym(reloc *CodeReloc, f io.ReadSeeker, sym *goobj.Sym,
	syms map[string]*goobj.Sym, symMap map[string]int,
	gcObjs map[string]uintptr, fileTabOffsetMap map[string]int) int {

	if curSymOffset, ok := symMap[sym.Name]; ok {
		return curSymOffset
	}

	var rsym SymData
	rsym.Name = sym.Name
	rsym.Kind = int(sym.Kind)
	curSymOffset := addSym(symMap, &reloc.Syms, &rsym)

	code := make([]byte, sym.Data.Size)
	f.Seek(sym.Data.Offset, io.SeekStart)
	_, err := f.Read(code)
	mustOK(err)
	switch int(sym.Kind) {
	case STEXT:
		rsym.Offset = len(reloc.Code)
		reloc.Code = append(reloc.Code, code...)
		readFuncData(&reloc.Mod, sym, f, syms, gcObjs,
			fileTabOffsetMap, curSymOffset, rsym.Offset)
	default:
		rsym.Offset = len(reloc.Data)
		reloc.Data = append(reloc.Data, code...)
	}
	addSym(symMap, &reloc.Syms, &rsym)

	for _, re := range sym.Reloc {
		symOff := -1
		if s, ok := syms[re.Sym.Name]; ok {
			symOff = relocSym(reloc, f, s, syms, symMap,
				gcObjs, fileTabOffsetMap)
		} else {
			var exSym SymData
			exSym.Name = re.Sym.Name
			exSym.Offset = -1
			if re.Type == R_TLS_LE {
				exSym.Name = TLSNAME
				exSym.Offset = re.Offset
			}
			if re.Type == R_CALLIND {
				exSym.Offset = 0
				exSym.Name = "R_CALLIND"
			}
			symOff = addSym(symMap, &reloc.Syms, &exSym)
		}
		rsym.Reloc = append(rsym.Reloc,
			Reloc{Offset: re.Offset + rsym.Offset, SymOff: symOff,
				Type: int(re.Type),
				Size: re.Size, Add: re.Add})
	}
	reloc.Syms[curSymOffset].Reloc = rsym.Reloc

	return curSymOffset
}

func strWrite(buf *bytes.Buffer, str ...string) {
	for _, s := range str {
		buf.WriteString(s)
		if s != "\n" {
			buf.WriteString(" ")
		}
	}
}

func Load(code *CodeReloc, symPtr map[string]uintptr) (*CodeModule, error) {
	codeLen := len(code.Code) + len(code.Data)
	codeByte, err := mmap(codeLen)
	if err != nil {
		return nil, err
	}

	var codeModule = CodeModule{
		Syms: make(map[string]uintptr),
	}
	var errBuf bytes.Buffer

	base := int((*sliceHeader)(unsafe.Pointer(&codeByte)).Data)
	dataBase := base + len(code.Code)

	var symAddrs = make([]int, len(code.Syms))

	for i, sym := range code.Syms {
		if sym.Offset == -1 || strings.HasPrefix(sym.Name, "go.itab") {
			if ptr, ok := symPtr[sym.Name]; ok {
				symAddrs[i] = int(ptr)
			} else {
				symAddrs[i] = -1
				strWrite(&errBuf, "unresolve external:", sym.Name, "\n")
			}
		} else if sym.Name == TLSNAME {
			RegTLS(symPtr, sym.Offset)
		} else if sym.Kind == STEXT {
			symAddrs[i] = code.Syms[i].Offset + base
			codeModule.Syms[sym.Name] = uintptr(symAddrs[i])
		} else {
			symAddrs[i] = code.Syms[i].Offset + dataBase
		}
	}

	for _, curSym := range code.Syms {
		for _, loc := range curSym.Reloc {
			sym := code.Syms[loc.SymOff]
			if symAddrs[loc.SymOff] == -1 {
				continue
			}

			var offset int
			switch loc.Type {
			case R_TLS_LE:
				binary.LittleEndian.PutUint32(code.Code[loc.Offset:], uint32(symPtr[TLSNAME]))
				continue
			case R_CALL, R_PCREL:
				var relocByte = code.Data
				var addrBase = dataBase
				if curSym.Kind == STEXT {
					addrBase = base
					relocByte = code.Code
				}
				offset = symAddrs[loc.SymOff] - (addrBase + loc.Offset + loc.Size) + loc.Add
				if offset > 2147483647 || offset < -2147483647 {
					errBuf.WriteString(fmt.Sprint("offset overflow:", offset, "sym:", sym.Name, "\n"))
				}
				// fmt.Println(curSym.Name, sym.Name, addrBase, symAddrs[loc.SymOff], offset, loc.Type, loc.Offset)
				binary.LittleEndian.PutUint32(relocByte[loc.Offset:], uint32(offset))
			case R_ADDR:
				var relocByte = code.Data
				if curSym.Kind == STEXT {
					relocByte = code.Code
				}
				offset = symAddrs[loc.SymOff] + loc.Add
				// fmt.Println(curSym.Name, sym.Name, sym.Offset, symAddrs[loc.SymOff], loc.Add, offset)
				*(*uintptr)(unsafe.Pointer(&(relocByte[loc.Offset:][0]))) = uintptr(offset)
			case R_CALLIND:

			case R_ADDROFF, R_WEAKADDROFF, R_METHODOFF:
				var relocByte = code.Data
				var addrBase = dataBase
				if curSym.Kind == STEXT {
					addrBase = base
					relocByte = code.Code
				}
				offset = symAddrs[loc.SymOff] - addrBase + loc.Add
				binary.LittleEndian.PutUint32(relocByte[loc.Offset:], uint32(offset))
			default:
				// fmt.Println("unknown reloc type:", loc.Type, sym.Name)
				strWrite(&errBuf, "unknown reloc type:", strconv.Itoa(loc.Type), sym.Name, "\n")
			}

		}
	}

	var module moduledata
	module.ftab = make([]functab, len(code.Mod.ftab))
	copy(module.ftab, code.Mod.ftab)
	pclnOff := len(code.Mod.pclntable)
	module.pclntable = make([]byte, len(code.Mod.pclntable)+
		(_funcSize+100)*len(code.Mod.ftab))
	copy(module.pclntable, code.Mod.pclntable)
	module.findfunctab = (uintptr)(unsafe.Pointer(&code.Mod.pcfunc[0]))
	module.minpc = (uintptr)(unsafe.Pointer(&codeByte[0]))
	module.maxpc = (uintptr)(unsafe.Pointer(&codeByte[len(code.Code)-1])) + 2
	module.filetab = code.Mod.filetab
	module.typemap = make(map[typeOff]uintptr)
	module.types = uintptr(dataBase)
	module.etypes = uintptr(dataBase + len(code.Data))
	module.text = uintptr(base)
	module.etext = uintptr(base + len(code.Code))
	codeModule.pcfuncdata = code.Mod.pcfunc // hold reference
	codeModule.stkmaps = code.Mod.stkmaps
	for i := range module.ftab {
		if i == 0 {
			continue
		}

		module.ftab[i].entry = uintptr(symAddrs[int(code.Mod.ftab[i].entry)])

		ptr2 := (uintptr)(unsafe.Pointer(&module.pclntable[pclnOff]))
		if PtrSize == 8 && ptr2&4 != 0 {
			pclnOff += 4
		}
		module.ftab[i].funcoff = uintptr(pclnOff)
		fi := &code.Mod.funcinfo[i-1]
		fi.entry = module.ftab[i].entry
		copy2Slice(module.pclntable[pclnOff:],
			unsafe.Pointer(&fi._func), _funcSize)
		pclnOff += _funcSize

		if len(fi.pcdata) > 0 {
			size := int(4 * fi.npcdata)
			copy2Slice(module.pclntable[pclnOff:],
				unsafe.Pointer(&fi.pcdata[0]), size)
			pclnOff += size
		}

		for i, v := range fi.funcdata {
			fi.funcdata[i] = (uintptr)(unsafe.Pointer(&(code.Mod.stkmaps[v][0])))
		}
		ptr := (uintptr)(unsafe.Pointer(&module.pclntable[pclnOff-1])) + 1
		if PtrSize == 8 && ptr&4 != 0 {
			t := [4]byte{}
			copy(module.pclntable[pclnOff:], t[:])
			pclnOff += len(t)
		}
		funcDataSize := int(PtrSize * fi.nfuncdata)
		copy2Slice(module.pclntable[pclnOff:],
			unsafe.Pointer(&fi.funcdata[0]), funcDataSize)
		pclnOff += funcDataSize

	}
	module.pclntable = module.pclntable[:pclnOff]
	if len(module.ftab) >= 2 {
		module.ftab[0] = module.ftab[1]
	}

	modulesLock.Lock()
	var firstModule = &firstmoduledata
	tmpModule = &module
	if fmodule != nil {
		firstModule = fmodule
	} else {
		fmodule = tmpModule
	}
	for datap := firstModule; ; {
		nextdatap := datap.next
		if nextdatap == nil {
			datap.next = tmpModule
			break
		}
		datap = nextdatap
	}
	codeModule.Module = tmpModule
	tmpModule = nil
	modulesLock.Unlock()

	copy(codeByte, code.Code)
	copy(codeByte[len(code.Code):], code.Data)
	codeModule.CodeByte = codeByte
	if errBuf.Len() > 0 {
		return &codeModule, errors.New(errBuf.String())
	}
	return &codeModule, nil
}

func copy2Slice(dst []byte, src unsafe.Pointer, size int) {
	var s = sliceHeader{
		Data: (uintptr)(src),
		Len:  size,
		Cap:  size,
	}
	copy(dst, *(*[]byte)(unsafe.Pointer(&s)))
}

func (cm *CodeModule) Unload() {
	runtime.GC()
	modulesLock.Lock()
	prevp := &firstmoduledata
	for datap := &firstmoduledata; datap != nil; {
		nextdatap := datap.next
		if datap == cm.Module {
			prevp.next = nextdatap
		}
		prevp = datap
		datap = nextdatap
	}
	if fmodule == cm.Module && fmodule != nil {
		fmodule = fmodule.next
	}
	modulesLock.Unlock()
	munmap(cm.CodeByte)
}
