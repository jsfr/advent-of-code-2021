using DelimitedFiles

lines = map(collect, readlines("input.txt"))
grid = permutedims(hcat(lines...))
counts = permutedims(hcat(
    map(col -> count(i -> i == '0', col), eachcol(grid)),
    map(col -> count(i -> i == '1', col), eachcol(grid))
))
gamma_binary = join(map(col -> if col[1] > col[2] 0 else 1 end, eachcol(counts)))
epsilon_binary = join(map(col -> if col[1] > col[2] 1 else 0 end, eachcol(counts)))
gamma = parse(Int, gamma_binary, base = 2)
epsilon = parse(Int, epsilon_binary, base = 2)

result = gamma * epsilon

print(result)
