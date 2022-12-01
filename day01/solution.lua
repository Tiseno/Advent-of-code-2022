local function P(any) print(vim.inspect(any)) end

local function read_input(name)
    local elf_list = {}
    local snack_list = {}
    for line in io.lines(name) do
        if line == "" then
            table.insert(elf_list, snack_list)
            snack_list = {}
        else
            table.insert(snack_list, line)
        end
    end
    return elf_list
end

local function sum(list)
    local s = 0
    for _, e in ipairs(list) do s = s + e end
    return s
end

local function biggest_snacker(nested_list)
    local largest = 0
    for _, e in ipairs(nested_list) do
        local s = sum(e)
        if s > largest then largest = sum(e) end
    end
    return largest
end

print("Problem 1:", biggest_snacker(read_input("input1.txt")))

local function map(list, fn)
    local result = {}
    for _, e in ipairs(list) do table.insert(result, fn(e)) end
    return result
end

local function three_biggest_snackers(nested_list)
    local elf_totals = map(nested_list, sum)
    table.sort(elf_totals)
    return {
        elf_totals[#elf_totals], elf_totals[#elf_totals - 1],
        elf_totals[#elf_totals - 2]
    }
end

print("Problem 2:", sum(three_biggest_snackers(read_input("input2.txt"))))
