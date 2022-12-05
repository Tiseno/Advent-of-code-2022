local lh = dofile "../lua/help.lua"

local function remove_crap(str) return str:gsub('%W', '') end

local function read_input(filename)
    local file = io.lines(filename)
    local line = file()
    local stacks = lh.map(function() return {} end, lh.split_string(line))
    line = file()
    while #line > 0 do
        local box_level = lh.split_string_chunks(4, line)
        for i, e in ipairs(box_level) do
            if e ~= "    " then
                local clean = remove_crap(e)
                table.insert(stacks[i], clean)
            end
        end
        line = file()
    end
    line = file()
    local instructions = {}
    while line do
        table.insert(instructions, lh.split_string(line))
        line = file()
    end
    return stacks, instructions
end

local function move_from_to_9000(stacks, n, from, to)
    if n == 0 then return end
    local element = table.remove(stacks[from], #(stacks[from]))
    table.insert(stacks[to], element)
    return move_from_to_9000(stacks, n - 1, from, to)
end

local function tops_of_stacks(stacks)
    local str = ""
    for _, e in ipairs(stacks) do if #e > 0 then str = str .. e[#e] end end
    return str
end

local function solution1(filename)
    local stacks, instructions = read_input(filename)
    for _, instruction in ipairs(instructions) do
        move_from_to_9000(stacks, tonumber(instruction[2]),
                          tonumber(instruction[4]), tonumber(instruction[6]))
    end
    return tops_of_stacks(stacks)
end

lh.P(solution1("input2.txt"))

local function move_from_to_9001_r(stacks, n, from, to, position)
    if n == 0 then return end
    local element = table.remove(stacks[from], #(stacks[from]))
    table.insert(stacks[to], position, element)
    return move_from_to_9001_r(stacks, n - 1, from, to, position)
end

local function move_from_to_9001(stacks, n, from, to)
    return move_from_to_9001_r(stacks, n, from, to, #stacks[to] + 1)
end

local function solution2(filename)
    local stacks, instructions = read_input(filename)
    for _, instruction in ipairs(instructions) do
        move_from_to_9001(stacks, tonumber(instruction[2]),
                          tonumber(instruction[4]), tonumber(instruction[6]))
    end
    return tops_of_stacks(stacks)
end

lh.P(solution2("input2.txt"))
