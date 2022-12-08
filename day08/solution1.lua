local lh = dofile "../lua/help.lua"

local function read_grid(name)
    local grid = {}
    for line in io.lines(name) do
        table.insert(grid, lh.string_to_number_list(line))
    end
    return grid
end

local function print_visible(grid)
    for i = 1, #grid do
        for j = 1, #grid do
            if grid[i][j] then
                io.write("S")
            else
                io.write(".")
            end
        end
        print()
    end
end

local function solution1(grid)
    local grid_visible = lh.map(function(list)
        return lh.map(function() return false end, list)
    end, grid)

    local tallest = -1;
    for i = 1, #grid do
        tallest = -1
        for j = 1, #grid do
            if grid[i][j] > tallest then
                tallest = grid[i][j]
                grid_visible[i][j] = true
            end
        end
        tallest = -1
        for j = #grid, 1, -1 do
            if grid[i][j] > tallest then
                tallest = grid[i][j]
                grid_visible[i][j] = true
            end
        end
    end
    for j = 1, #grid do
        tallest = -1
        for i = 1, #grid do
            if grid[i][j] > tallest then
                tallest = grid[i][j]
                grid_visible[i][j] = true
            end
        end
        tallest = -1
        for i = #grid, 1, -1 do
            if grid[i][j] > tallest then
                tallest = grid[i][j]
                grid_visible[i][j] = true
            end
        end
    end

    print_visible(grid_visible)

    lh.P(lh.sum(lh.map(function(row)
        return lh.count(function(value) return value end, row)
    end, grid_visible)))
end

solution1(read_grid("input2.txt"))
