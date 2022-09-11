package gta

//import ("fmt")

func gta(limit int, nums ...int) int {
	return 0
}

func Map[T, U any](s []T, f func(T) U) []U {
	r := make([]U, len(s))
	for i, v := range s {
		r[i] = f(v)
	}
	return r
}

func contains(s []int, e int) bool {
	for _, a := range s {
		if a == e {
			return true
		}
	}
	return false
}

func findMinAndMax(a []int) (min int, max int) {
	min = a[0]
	max = a[0]
	for _, value := range a {
		if value < min {
			min = value
		}
		if value > max {
			max = value
		}
	}
	return min, max
}

func prependInt(x []int, y int) []int {
	x = append(x, 0)
	copy(x[1:], x)
	x[0] = y
	return x
}

func splitNumber(num int) []int {
	mod := num % 10
	result := []int{mod}
	div := num / 10
	for div != 0 {
		mod = div % 10
		result = prependInt(result, mod)
		div = div / 10
	}
	return result
}

// // createBaseArray ...
func CreateBaseArray(limit int, nums ...int) []int {
	result := make([]int, limit)
	inserted := 0
	numsSplitted := Map[int, []int](nums, splitNumber)
	_, maxLength := findMinAndMax(
		Map[[]int, int](
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

					if !contains(result, val) {

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
