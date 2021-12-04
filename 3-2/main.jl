using DelimitedFiles

function most_common_bit(vec) 
    zeros = count(i -> i == '0', vec)
    ones = count(i -> i == '1', vec)

    if ones >= zeros
        return '1'
    else
        return '0'
    end
end

function least_common_bit(vec) 
    zeros = count(i -> i == '0', vec)
    ones = count(i -> i == '1', vec)

    if ones >= zeros
        return '0'
    else
        return '1'
    end
end

function filter_grid(grid, column_index, filter_fn)
    filter_value = filter_fn(grid[:,column_index])
    new_grid = grid[grid[:,column_index] .== filter_value, :]
    return new_grid
end

lines = map(collect, readlines("input.txt"))
grid = permutedims(hcat(lines...))

o2_generator_rating_binary = copy(grid)
co2_scrubber_rating_binary = copy(grid)

column_index = 1
while size(o2_generator_rating_binary, 1) > 1
    global o2_generator_rating_binary = filter_grid(o2_generator_rating_binary, column_index, most_common_bit)
    global column_index = column_index + 1
end

column_index = 1
while size(co2_scrubber_rating_binary, 1) > 1
    global co2_scrubber_rating_binary = filter_grid(co2_scrubber_rating_binary, column_index, least_common_bit)
    global column_index = column_index + 1
end

o2_generator_rating = parse(Int, join(o2_generator_rating_binary), base = 2)
co2_scrubber_rating = parse(Int, join(co2_scrubber_rating_binary), base = 2)

result = o2_generator_rating * co2_scrubber_rating

print(o2_generator_rating, "\n")
print(co2_scrubber_rating, "\n")
print(result)
