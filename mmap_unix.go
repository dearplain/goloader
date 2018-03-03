// +build darwin dragonfly freebsd linux openbsd solaris netbsd

package goloader

import "syscall"

func mmap(size int) ([]byte, error) {
	return syscall.Mmap(
		-1,
		0,
		size,
		syscall.PROT_READ|syscall.PROT_WRITE|syscall.PROT_EXEC,
		syscall.MAP_ANON)
}

func munmap(b []byte) (err error) {
	return syscall.Munmap(b)
}
