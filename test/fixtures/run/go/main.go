package main

import "fmt"

func twice(x int) int {
	return x * 2
}

func main() {
	fmt.Println("twice 21 is", twice(21))
}
