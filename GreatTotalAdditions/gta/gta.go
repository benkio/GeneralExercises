package gta

import (
	base "benkio/greattotaladditions/base"
)

func Gta(limit int, nums ...int) int {
	baseArray := CreateBaseArray(limit, nums...)
	combinations := base.Combinations(baseArray)
	sums := base.Map[[]int, int](combinations, func(arr []int) int {
		r := 0
		for _, elem := range arr { r += elem }
		return r
	})
	result := 0
	for _, sum := range sums { result += sum }
	return result
}

func SplitNumber(num int) []int {
	mod := num % 10
	result := []int{mod}
	div := num / 10
	for div != 0 {
		mod = div % 10
		result = base.PrependInt(result, mod)
		div = div / 10
	}
	return result
}

// // createBaseArray ...
func CreateBaseArray(limit int, nums ...int) []int {
	result := make([]int, limit)
	for i, _ := range result {
		result[i] = -1
	}
	inserted := 0
	numsSplitted := base.Map[int, []int](nums, SplitNumber)
	_, maxLength := base.FindMinAndMax(
		base.Map[[]int, int](
			numsSplitted,
			func(x []int) int {
				return len(x)
			}))
	for inserted < limit {
		for i := 0; i < maxLength; i++ {
			for j := 0; j < len(numsSplitted); j++ {
				numSplit := numsSplitted[j]

				if i < len(numSplit) {
					val := numSplit[i]

					if !base.Contains(result, val) {

						result[inserted] = val
						inserted++
					}
				}
				if inserted == limit {
					break
				}
			}
			if inserted == limit {
				break
			}
		}
	}
	return result
}
