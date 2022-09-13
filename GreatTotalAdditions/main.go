package main

import (
	"fmt"
	gta "benkio/greattotaladditions/gta"
	base "benkio/greattotaladditions/base"
)

func main() {
	fmt.Printf("example 1 %#v\n", gta.CreateBaseArray(7, 123489, 5, 67))
	fmt.Printf("example 2 %#v\n", gta.CreateBaseArray(8, 12348, 47, 3639))

	fmt.Printf("permutation example: %#v\n", base.Permutations([]int {1,2,3}))
	fmt.Printf("permutation example: %#v\n", len(base.Permutations([]int {1,2,3,4})))

}
