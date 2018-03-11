package main

import (
	"fmt"
	"runtime"
)

type Vertex struct {
	X, Y int
}

func (v *Vertex) Print() {
	fmt.Println("print", v)
}

type PrintInf interface {
	Print()
}

func main() {

	var (
		v1 = Vertex{1, 2}
		v2 = Vertex{X: 1}
		v3 = Vertex{}
		p  = &Vertex{1, 2}
	)

	fmt.Println(Vertex{1, 2})
	fmt.Println(v1, p, v2, v3)
	fmt.Printf("%#v %#v %#v %#v\n", v1, p, v2, v3)

	var inf PrintInf = p
	inf.Print()

	{
		names := [4]string{
			"John",
			"Paul",
			"George",
			"Ringo",
		}
		fmt.Println(names)
	}

	{
		s := []struct {
			i int
			b bool
		}{
			{2, true},
			{3, false},
		}
		fmt.Println(s)
	}

	{
		var m = map[string]Vertex{
			"Bell Labs": Vertex{
				40, -74,
			},
			"Google": Vertex{
				37, -122,
			},
		}
		fmt.Println(m)
	}

	{
		whatAmI := func(i interface{}) {
			switch t := i.(type) {
			case bool:
				fmt.Println("I'm a bool")
			case int:
				fmt.Println("I'm an int")
			default:
				fmt.Printf("Don't know type %T\n", t)
			}
		}
		go whatAmI(true)
		go whatAmI(1)
		whatAmI("hey")
	}

	recoverTest()

	{
		pos, neg := adder(), adder()
		for i := 0; i < 10; i++ {
			fmt.Println(
				pos(i),
				neg(-2*i),
			)
		}
	}

}

func adder() func(int) int {
	sum := 0
	return func(x int) int {
		sum += x
		return sum
	}
}

func recoverTest() {
	defer logPanic()
	panic("this is a panic test")
}

func logPanic() {
	if r := recover(); r != nil {
		trace := make([]byte, 1024)
		count := runtime.Stack(trace, false)
		fmt.Printf("panic: %s\nStack of %d bytes:\n%s\n", r, count, trace)
	}
}
