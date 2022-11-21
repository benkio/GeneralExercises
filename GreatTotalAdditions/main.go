package main

import (
	"fmt"
	gta "benkio/greattotaladditions/gta"
)

func main() {
	fmt.Printf("kata example: %d\n", gta.Gta(7, 123489, 5, 67))
	fmt.Printf("kata example 2 (expected 27): %d\n", gta.Gta(2, 9023, 9591))
}
