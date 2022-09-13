package gta

import (
	base "benkio/greattotaladditions/base"
)

func gta(limit int, nums ...int) int {
	return 0
}

func splitNumber(num int) []int {
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
	inserted := 0
	numsSplitted := base.Map[int, []int](nums, splitNumber)
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
