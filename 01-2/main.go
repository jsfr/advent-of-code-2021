package main

import (
    "bufio"
    "fmt"
    "log"
    "os"
    "strconv"
)

const slidingWindowSize = 3

func main() {
    file, err := os.Open("input.txt")
    if err != nil {
        log.Fatal(err)
    }
    defer file.Close()

    scanner := bufio.NewScanner(file)

    // Super hack to not count first "increase"
    slidingWindowA := make([]int64, 0, slidingWindowSize)
    slidingWindowB := make([]int64, 0, slidingWindowSize)
    increasesInDepth := 0

    for scanner.Scan() {
        nextMeasurement, err := strconv.ParseInt(scanner.Text(), 10, 64)

        if err != nil {
            log.Fatal(err)
        }

        if len(slidingWindowB) >= slidingWindowSize {
            slidingWindowB = slidingWindowB[1:]
        }

        slidingWindowB = append(slidingWindowB, nextMeasurement)

        if len(slidingWindowA) == slidingWindowSize && len(slidingWindowB) == slidingWindowSize {
            sumA := sum(slidingWindowA)
            sumB := sum(slidingWindowB)

            if sumB > sumA {
                increasesInDepth++
            }
        }


        if len(slidingWindowA) >= slidingWindowSize {
            slidingWindowA = slidingWindowA[1:]
        }

        slidingWindowA = append(slidingWindowA, nextMeasurement)
    }

    fmt.Println(increasesInDepth)
}

func sum(array []int64) int64 {  
    var result int64 = 0  
    for _, v := range array {  
        result += v  
    }  
    return result  
}
