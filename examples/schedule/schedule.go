package main

import (
	"fmt"
	"runtime"
	"sync"
	"time"
)

func main() {

	var ch = make(chan bool, 20000)
	var begin = make(chan bool)
	var closeCh = make(chan bool)

	w := sync.WaitGroup{}

	w.Add(1)
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
		close(closeCh)
		w.Done()
	}()

	for i := 0; i < 50000; i++ {
		w.Add(1)
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
				select {
				case <-closeCh:
					w.Done()
					return
				default:
				}
			}
		}()
	}

	for i := 0; i < 20; i++ {
		w.Add(1)
		go func() {
			defer logPanic()
			for {
				select {
				case <-closeCh:
					w.Done()
					return
				case ch <- true:
				}
			}
		}()
	}

	fmt.Println("all start")
	begin <- true

	w.Wait()
}

func logPanic() {
	if r := recover(); r != nil {
		trace := make([]byte, 1024)
		count := runtime.Stack(trace, false)
		fmt.Printf("Stack of %d bytes: %s\n", count, trace)
	}
}
