package main

import (
	"fmt"
	"os"
	"runtime"
	"time"
)

func main() {

	var ch = make(chan bool, 20000)
	var begin = make(chan bool)

	go func() {
		defer logPanic()
		runtime.LockOSThread()
		<-begin
		fmt.Println("begin")
		tm := time.Now()
		for i := 0; i < 10000000; i++ {
			<-ch
		}
		fmt.Println(time.Now().Sub(tm))
		os.Exit(0)
	}()

	for i := 0; i < 50000; i++ {
		go func() {
			defer logPanic()
			var count int
			load := 100
			for {
				count++
				re := fmt.Sprint("haha")
				if re == "haha" && count >= load {
					count = 0
					runtime.Gosched()
				}
			}
		}()
	}

	for i := 0; i < 20; i++ {
		go func() {
			defer logPanic()
			for {
				ch <- true
			}
		}()
	}

	fmt.Println("all start")
	begin <- true

	select {}
}

func logPanic() {
	if r := recover(); r != nil {
		trace := make([]byte, 1024)
		count := runtime.Stack(trace, false)
		fmt.Printf("Stack of %d bytes: %s\n", count, trace)
	}
}
