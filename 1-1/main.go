package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
    "strconv"
)

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

    // Super hack to not count first "increase"
    var previousDepth int64 = -1
    increasesInDepth := -1

	for scanner.Scan() {
        currentDepth, err := strconv.ParseInt(scanner.Text(), 10, 64)

        if err != nil {
            log.Fatal(err)
        }

        if currentDepth > previousDepth {
            increasesInDepth += 1
        }

        previousDepth = currentDepth
	}

    fmt.Println(increasesInDepth)
}
