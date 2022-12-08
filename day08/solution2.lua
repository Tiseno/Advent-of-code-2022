local lh = dofile "../lua/help.lua"

local function read_grid(name)
    local grid = {}
    for line in io.lines(name) do
        table.insert(grid, lh.string_to_number_list(line))
    end
    return grid
end

local function scenic_score_of_tree(grid, i, j)
    local tree_height = grid[i][j]
    local right_score, left_score, up_score, down_score = 0, 0, 0, 0
    for k = i + 1, #grid do
        right_score = right_score + 1
        if grid[k][j] >= tree_height then break end
    end
    for k = i - 1, 1, -1 do
        left_score = left_score + 1
        if grid[k][j] >= tree_height then break end
    end
    for k = j + 1, #grid do
        down_score = down_score + 1
        if grid[i][k] >= tree_height then break end
    end
    for k = j - 1, 1, -1 do
        up_score = up_score + 1
        if grid[i][k] >= tree_height then break end
    end
    return right_score * left_score * up_score * down_score
end

local function solution2(grid)
    local highest_scenic_score = 0

    for i = 1, #grid do
        for j = 1, #grid do
            local score = scenic_score_of_tree(grid, i, j)
            if score > highest_scenic_score then
                highest_scenic_score = score
            end
        end
    end

    print(highest_scenic_score)
end

solution2(read_grid("input2.txt"))
