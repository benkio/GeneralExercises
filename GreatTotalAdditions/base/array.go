package base

func Contains(s []int, e int) bool {
        for _, a := range s {
                if a == e {
                        return true
                }
        }
        return false
}

func FindMinAndMax(a []int) (min int, max int) {
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

func PrependInt(x []int, y int) []int {
        x = append(x, 0)
        copy(x[1:], x)
        x[0] = y
        return x
}

func RemoveCopy(slice []int, i int) []int {
	a := make([]int, 0)
	for j, elem := range slice {
		if j == i {
			continue
		} else {
			a = append(a, elem)
		}
	}
	return a
}

func Permutations(xs []int) [][]int {
        return internalPermutation(make([][]int, 0), make([]int, 0), xs)
}

func internalPermutation(acc [][]int, curPerm []int, rest []int) [][]int {
	if len(rest) == 0 {
		return append(acc, curPerm)
	} else {
		result := make([][]int, 0)
		for i := 0; i < len(rest); i++ {
			nextPermutation := append(curPerm, rest[i])
			remaining := RemoveCopy(rest, i)
			otherPermutations := internalPermutation(acc, nextPermutation, remaining)
			result = append(result, otherPermutations...)
		}
		return result;
	}
}
