package goloader

import (
	"bytes"
	"cmd/objfile/goobj"
	"encoding/binary"
	"errors"
	"fmt"
	"io"
	"os"
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
	// R_ADDRARM64 relocates an adrp, add pair to compute the address of the
	// referenced symbol.
	R_ADDRARM64 = 3
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

// CodeReloc dispatch and load CodeReloc struct via network is OK
type CodeReloc struct {
	Code []byte
	Data []byte
	Mod  Module
	Syms []SymData
}

type CodeModule struct {
	Syms       map[string]uintptr
	CodeByte   []byte
	Module     interface{}
	pcfuncdata []findfuncbucket
	stkmaps    [][]byte
	itabs      []itabReloc
	itabSyms   []itabSym
	typemap    map[typeOff]uintptr
}

type itabSym struct {
	ptr   int
	inter int
	_type int
}

type itabReloc struct {
	locOff  int
	symOff  int
	size    int
	locType int
	add     int
}

type symFile struct {
	sym  *goobj.Sym
	file *os.File
}

var (
	tmpModule   interface{}
	modules     = make(map[interface{}]bool)
	modulesLock sync.Mutex
	mov32bit    = [8]byte{0x00, 0x00, 0x80, 0xD2, 0x00, 0x00, 0xA0, 0xF2}
)

func ReadObj(f *os.File) (*CodeReloc, error) {
	obj, err := goobj.Parse(f, "main")
	if err != nil {
		return nil, fmt.Errorf("read error: %v", err)
	}

	var syms = make(map[string]symFile)
	for _, sym := range obj.Syms {
		syms[sym.Name] = symFile{
			sym:  sym,
			file: f,
		}
	}

	var symMap = make(map[string]int)
	var gcObjs = make(map[string]uintptr)
	var fileTabOffsetMap = make(map[string]int)

	var reloc CodeReloc

	for _, sym := range obj.Syms {
		if sym.Kind == STEXT {
			relocSym(&reloc, symFile{sym: sym,
				file: f}, syms, symMap,
				gcObjs, fileTabOffsetMap)
		}
	}

	return &reloc, nil
}

func ReadObjs(files []string, pkgPath []string) (*CodeReloc, error) {

	var fs []*os.File
	for _, file := range files {
		f, err := os.Open(file)
		if err != nil {
			return nil, err
		}
		fs = append(fs, f)
		defer f.Close()
	}

	var allSyms = make(map[string]symFile)

	var symMap = make(map[string]int)
	var gcObjs = make(map[string]uintptr)
	var fileTabOffsetMap = make(map[string]int)

	var reloc CodeReloc

	var goObjs []*goobj.Package
	for i, f := range fs {
		if pkgPath[i] == "" {
			pkgPath[i] = "main"
		}
		obj, err := goobj.Parse(f, pkgPath[i])
		if err != nil {
			return nil, fmt.Errorf("read error: %v", err)
		}

		for _, sym := range obj.Syms {
			allSyms[sym.Name] = symFile{
				sym:  sym,
				file: f,
			}
		}
		goObjs = append(goObjs, obj)
	}

	for i, obj := range goObjs {
		for _, sym := range obj.Syms {
			if sym.Kind == STEXT {
				relocSym(&reloc, symFile{sym: sym,
					file: fs[i]}, allSyms, symMap,
					gcObjs, fileTabOffsetMap)
			}
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

func relocSym(reloc *CodeReloc, curSym symFile,
	allSyms map[string]symFile, symMap map[string]int,
	gcObjs map[string]uintptr, fileTabOffsetMap map[string]int) int {

	if curSymOffset, ok := symMap[curSym.sym.Name]; ok {
		return curSymOffset
	}

	var rsym SymData
	rsym.Name = curSym.sym.Name
	rsym.Kind = int(curSym.sym.Kind)
	curSymOffset := addSym(symMap, &reloc.Syms, &rsym)

	code := make([]byte, curSym.sym.Data.Size)
	curSym.file.Seek(curSym.sym.Data.Offset, io.SeekStart)
	_, err := curSym.file.Read(code)
	mustOK(err)
	switch int(curSym.sym.Kind) {
	case STEXT:
		rsym.Offset = len(reloc.Code)
		reloc.Code = append(reloc.Code, code...)
		readFuncData(&reloc.Mod, curSym, allSyms, gcObjs,
			fileTabOffsetMap, curSymOffset, rsym.Offset)
	default:
		rsym.Offset = len(reloc.Data)
		reloc.Data = append(reloc.Data, code...)
	}
	addSym(symMap, &reloc.Syms, &rsym)

	for _, re := range curSym.sym.Reloc {
		symOff := -1
		if s, ok := allSyms[re.Sym.Name]; ok {
			symOff = relocSym(reloc, s, allSyms, symMap,
				gcObjs, fileTabOffsetMap)
		} else {
			var exSym SymData
			exSym.Name = re.Sym.Name
			exSym.Offset = -1
			if re.Type == R_TLS_LE {
				exSym.Name = TLSNAME
				exSym.Offset = int(re.Offset)
			}
			if re.Type == R_CALLIND {
				exSym.Offset = 0
				exSym.Name = R_CALLIND_NAME
			}
			if strings.HasPrefix(exSym.Name, "type..importpath.") {
				path := strings.TrimLeft(exSym.Name, "type..importpath.")
				path = strings.Trim(path, ".")
				pathb := []byte(path)
				pathb = append(pathb, 0)
				exSym.Offset = len(reloc.Data)
				reloc.Data = append(reloc.Data, pathb...)
			}
			symOff = addSym(symMap, &reloc.Syms, &exSym)
		}
		rsym.Reloc = append(rsym.Reloc,
			Reloc{Offset: int(re.Offset) + rsym.Offset, SymOff: symOff,
				Type: int(re.Type),
				Size: int(re.Size), Add: int(re.Add)})
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
	pCodeLen := len(code.Code) + len(code.Data)
	codeLen := int(float32(pCodeLen) * 1.5)
	codeByte, err := Mmap(codeLen)
	if err != nil {
		return nil, err
	}

	var codeModule = CodeModule{
		Syms:    make(map[string]uintptr),
		typemap: make(map[typeOff]uintptr),
	}
	var errBuf bytes.Buffer

	base := int((*sliceHeader)(unsafe.Pointer(&codeByte)).Data)
	dataBase := base + len(code.Code)

	var symAddrs = make([]int, len(code.Syms))
	var itabIndexs []int
	var funcTypeMap = make(map[string]*int)
	for i, sym := range code.Syms {
		if sym.Offset == -1 {
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
		} else if strings.HasPrefix(sym.Name, "go.itab") {
			if ptr, ok := symPtr[sym.Name]; ok {
				symAddrs[i] = int(ptr)
			} else {
				itabIndexs = append(itabIndexs, i)
			}
		} else {
			symAddrs[i] = code.Syms[i].Offset + dataBase

			if strings.HasPrefix(sym.Name, "type.func") {
				funcTypeMap[sym.Name] = &symAddrs[i]
			}
			if strings.HasPrefix(sym.Name, "type.") {
				if ptr, ok := symPtr[sym.Name]; ok {
					symAddrs[i] = int(ptr)
				}
			}
		}
	}

	var itabSymMap = make(map[string]int)
	for _, itabIndex := range itabIndexs {
		curSym := code.Syms[itabIndex]
		sym1 := symAddrs[curSym.Reloc[0].SymOff]
		sym2 := symAddrs[curSym.Reloc[1].SymOff]
		itabSymMap[curSym.Name] = len(codeModule.itabSyms)
		codeModule.itabSyms = append(codeModule.itabSyms, itabSym{inter: sym1, _type: sym2})

		if sym1 == -1 || sym2 == -1 {
			continue
		}
		addIFaceSubFuncType(funcTypeMap, codeModule.typemap,
			(*interfacetype)(unsafe.Pointer(uintptr(sym1))), base)
	}

	var armcode = []byte{0x04, 0xF0, 0x1F, 0xE5, 0x00, 0x00, 0x00, 0x00}
	var arm64code = []byte{0x43, 0x00, 0x00, 0x58, 0x60, 0x00, 0x1F, 0xD6, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00}
	var x86code = []byte{0xff, 0x25, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00}
	var movcode byte = 0x8b
	var leacode byte = 0x8d
	var cmplcode byte = 0x83
	var jmpcode byte = 0xe9
	var jmpOff = pCodeLen
	for _, curSym := range code.Syms {
		for _, loc := range curSym.Reloc {
			sym := code.Syms[loc.SymOff]
			if symAddrs[loc.SymOff] == -1 {
				continue
			}
			if symAddrs[loc.SymOff] == 0 && strings.HasPrefix(sym.Name, "go.itab") {
				codeModule.itabs = append(codeModule.itabs,
					itabReloc{locOff: loc.Offset, symOff: itabSymMap[sym.Name],
						size: loc.Size, locType: loc.Type, add: loc.Add})
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
				if offset > 0x7fffffff || offset < -0x8000000 {
					if jmpOff+8 > codeLen {
						strWrite(&errBuf, "len overflow", "sym:", sym.Name, "\n")
						continue
					}
					rb := relocByte[loc.Offset-2:]
					if loc.Type == R_CALL {
						offset = (base + jmpOff) - (addrBase + loc.Offset + loc.Size)
						copy(codeByte[jmpOff:], x86code)
						binary.LittleEndian.PutUint32(relocByte[loc.Offset:], uint32(offset))
						if uint64(symAddrs[loc.SymOff]+loc.Add) > 0xFFFFFFFF {
							binary.LittleEndian.PutUint64(codeByte[jmpOff+6:], uint64(symAddrs[loc.SymOff]+loc.Add))
						} else {
							binary.LittleEndian.PutUint32(codeByte[jmpOff+6:], uint32(symAddrs[loc.SymOff]+loc.Add))
						}
						jmpOff += len(x86code)
					} else if rb[0] == leacode || rb[0] == movcode || rb[0] == cmplcode || rb[1] == jmpcode {
						offset = (base + jmpOff) - (addrBase + loc.Offset + loc.Size)
						binary.LittleEndian.PutUint32(relocByte[loc.Offset:], uint32(offset))
						if rb[0] == leacode {
							rb[0] = movcode
						}
						if uint64(symAddrs[loc.SymOff]+loc.Add) > 0xFFFFFFFF {
							binary.LittleEndian.PutUint64(codeByte[jmpOff:], uint64(symAddrs[loc.SymOff]+loc.Add))
							jmpOff += 12
						} else {
							binary.LittleEndian.PutUint32(codeByte[jmpOff:], uint32(symAddrs[loc.SymOff]+loc.Add))
							jmpOff += 8
						}
					} else {
						strWrite(&errBuf, "offset overflow sym:", sym.Name, "\n")
						binary.LittleEndian.PutUint32(relocByte[loc.Offset:], uint32(offset))
					}
					continue
				}
				binary.LittleEndian.PutUint32(relocByte[loc.Offset:], uint32(offset))
			case R_CALLARM, R_CALLARM64:
				var add = loc.Add
				var pcOff = 0
				if loc.Type == R_CALLARM {
					add = loc.Add & 0xffffff
					if add > 256 {
						add = 0
					} else {
						add += 2
					}
					pcOff = 8
				}
				offset = (symAddrs[loc.SymOff] - (base + loc.Offset + pcOff) + add) / 4
				if offset > 0x7FFFFF || offset < -0x800000 {
					if jmpOff+4 > codeLen {
						strWrite(&errBuf, "len overflow", "sym:", sym.Name, "\n")
						continue
					}
					align := jmpOff % 4
					if align != 0 {
						jmpOff += (4 - align)
					}
					offset = (jmpOff - (loc.Offset + pcOff)) / 4
					var v = uint32(offset)
					b := code.Code[loc.Offset:]
					b[0] = byte(v)
					b[1] = byte(v >> 8)
					b[2] = byte(v >> 16)
					var jmpLocOff = 0
					var jmpLen = 0
					if loc.Type == R_CALLARM64 {
						copy(codeByte[jmpOff:], arm64code)
						jmpLen = len(arm64code)
						jmpLocOff = 8
					} else {
						copy(codeByte[jmpOff:], armcode)
						jmpLen = len(armcode)
						jmpLocOff = 4
					}
					*(*uintptr)(unsafe.Pointer(&(codeByte[jmpOff+jmpLocOff:][0]))) = uintptr(symAddrs[loc.SymOff] + add*4)
					jmpOff += jmpLen
					continue
				}
				var v = uint32(offset)
				b := code.Code[loc.Offset:]
				b[0] = byte(v)
				b[1] = byte(v >> 8)
				b[2] = byte(v >> 16)
			case R_ADDRARM64:
				if curSym.Kind != STEXT {
					strWrite(&errBuf, "not in code?\n")
				}
				relocADRP(code.Code[loc.Offset:], base+loc.Offset, symAddrs[loc.SymOff], sym.Name)
			case R_ADDR:
				var relocByte = code.Data
				if curSym.Kind == STEXT {
					relocByte = code.Code
				}
				offset = symAddrs[loc.SymOff] + loc.Add
				*(*uintptr)(unsafe.Pointer(&(relocByte[loc.Offset:][0]))) = uintptr(offset)
			case R_CALLIND:

			case R_ADDROFF, R_WEAKADDROFF, R_METHODOFF:
				var relocByte = code.Data
				var addrBase = base
				if curSym.Kind == STEXT {
					strWrite(&errBuf, "impossible!", sym.Name, "locate on code segment", "\n")
				}
				offset = symAddrs[loc.SymOff] - addrBase + loc.Add
				binary.LittleEndian.PutUint32(relocByte[loc.Offset:], uint32(offset))
			default:
				strWrite(&errBuf, "unknown reloc type:", strconv.Itoa(loc.Type), sym.Name, "\n")
			}

		}
	}

	var module moduledata
	module.ftab = make([]functab, len(code.Mod.ftab))
	copy(module.ftab, code.Mod.ftab)
	pclnOff := len(code.Mod.pclntable)
	module.pclntable = make([]byte, len(code.Mod.pclntable)+
		(_funcSize+256)*len(code.Mod.ftab))
	copy(module.pclntable, code.Mod.pclntable)
	module.findfunctab = (uintptr)(unsafe.Pointer(&code.Mod.pcfunc[0]))
	module.minpc = (uintptr)(unsafe.Pointer(&codeByte[0]))
	module.maxpc = (uintptr)(unsafe.Pointer(&codeByte[len(code.Code)-1])) + 2
	module.filetab = code.Mod.filetab
	module.typemap = codeModule.typemap
	module.types = uintptr(base)
	module.etypes = uintptr(base + codeLen)
	module.text = uintptr(base)
	module.etext = uintptr(base + len(code.Code))
	codeModule.pcfuncdata = code.Mod.pcfunc // hold reference
	codeModule.stkmaps = code.Mod.stkmaps
	for i := range module.ftab {
		module.ftab[i].entry = uintptr(symAddrs[int(code.Mod.ftab[i].entry)])

		ptr2 := (uintptr)(unsafe.Pointer(&module.pclntable[pclnOff]))
		if PtrSize == 8 && ptr2&4 != 0 {
			pclnOff += 4
		}
		module.ftab[i].funcoff = uintptr(pclnOff)
		fi := code.Mod.funcinfo[i]
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

		var funcdata = make([]uintptr, len(fi.funcdata))
		copy(funcdata, fi.funcdata)
		for i, v := range funcdata {
			if v != 0 {
				funcdata[i] = (uintptr)(unsafe.Pointer(&(code.Mod.stkmaps[v][0])))
			} else {
				funcdata[i] = (uintptr)(0)
			}
		}
		ptr := (uintptr)(unsafe.Pointer(&module.pclntable[pclnOff-1])) + 1
		if PtrSize == 8 && ptr&4 != 0 {
			t := [4]byte{}
			copy(module.pclntable[pclnOff:], t[:])
			pclnOff += len(t)
		}
		funcDataSize := int(PtrSize * fi.nfuncdata)
		copy2Slice(module.pclntable[pclnOff:],
			unsafe.Pointer(&funcdata[0]), funcDataSize)
		pclnOff += funcDataSize

	}
	module.pclntable = module.pclntable[:pclnOff]
	module.ftab = append(module.ftab, functab{})
	for i:= len(module.ftab)-1; i > 0; i-- { 
		module.ftab[i] = module.ftab[i-1]
	}
	module.ftab = append(module.ftab, functab{})
	module.ftab[0].entry = module.minpc
	module.ftab[len(module.ftab)-1].entry = module.maxpc 

	modulesLock.Lock()
	addModule(&codeModule, &module, runtime.Version())
	modulesLock.Unlock()

	copy(codeByte, code.Code)
	copy(codeByte[len(code.Code):], code.Data)
	codeModule.CodeByte = codeByte

	for i := range codeModule.itabSyms {
		it := &codeModule.itabSyms[i]
		if it.inter == -1 || it._type == -1 {
			continue
		}
		it.ptr = getitab(it.inter, it._type, false)
	}
	for _, it := range codeModule.itabs {
		symAddr := codeModule.itabSyms[it.symOff].ptr
		if symAddr == 0 {
			continue
		}
		switch it.locType {
		case R_PCREL:
			pc := base + it.locOff + it.size
			offset := symAddr - pc + it.add
			if offset > 0x7FFFFFFF || offset < -0x80000000 {
				offset = (base + jmpOff) - pc + it.add
				binary.LittleEndian.PutUint32(codeByte[it.locOff:], uint32(offset))
				codeByte[it.locOff-2:][0] = movcode
				*(*uintptr)(unsafe.Pointer(&(codeByte[jmpOff:][0]))) = uintptr(symAddr)
				jmpOff += PtrSize
				continue
			}
			binary.LittleEndian.PutUint32(codeByte[it.locOff:], uint32(offset))
		case R_ADDRARM64:
			relocADRP(codeByte[it.locOff:], base+it.locOff, symAddr, "unknown")
		}
	}

	if errBuf.Len() > 0 {
		return &codeModule, errors.New(errBuf.String())
	}
	return &codeModule, nil
}

func relocADRP(mCode []byte, pc int, symAddr int, symName string) {
	pcPage := pc - pc&0xfff
	lowOff := symAddr & 0xfff
	symPage := symAddr - lowOff
	pageOff := symPage - pcPage
	if pageOff > 1<<31 || pageOff < -1<<31 {
		// fmt.Println("adrp overflow!", symName, symAddr, symAddr < (1<<31))
		movlow := binary.LittleEndian.Uint32(mov32bit[:4])
		movhigh := binary.LittleEndian.Uint32(mov32bit[4:])
		adrp := binary.LittleEndian.Uint32(mCode)
		symAddrUint32 := uint32(symAddr)
		movlow = (((adrp & 0x1f) | movlow) | ((symAddrUint32 & 0xffff) << 5))
		movhigh = (((adrp & 0x1f) | movhigh) | ((symAddrUint32 & 0xffff0000) >> 16 << 5))
		// fmt.Println(adrp, movlow, movhigh)
		binary.LittleEndian.PutUint32(mCode, movlow)
		binary.LittleEndian.PutUint32(mCode[4:], movhigh)
		return
	}
	fmt.Println("pageOff<0:", pageOff < 0)
	// 2bit + 19bit + low(12bit) = 33bit
	pageAnd := (uint32((pageOff>>12)&3) << 29) | (uint32((pageOff>>15)&0x7ffff) << 5)

	adrp := binary.LittleEndian.Uint32(mCode)
	adrp = adrp | pageAnd
	binary.LittleEndian.PutUint32(mCode, adrp)

	lowOff = lowOff << 10
	adrpAdd := binary.LittleEndian.Uint32(mCode[4:])
	adrpAdd = adrpAdd | uint32(lowOff)
	binary.LittleEndian.PutUint32(mCode[4:], adrpAdd)
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
	removeModule(cm.Module, runtime.Version())
	modulesLock.Unlock()
	Munmap(cm.CodeByte)
}
