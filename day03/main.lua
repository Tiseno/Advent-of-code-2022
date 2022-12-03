local lh = dofile("../lua/help.lua")

local function read_rucksack_strings(filename)
    local rucksacks = {}
    for line in io.lines(filename) do table.insert(rucksacks, line) end
    return rucksacks
end

local function compartmentalize(rucksack_string)
    local compartment1 = lh.string_to_set(
                             rucksack_string:sub(1, #rucksack_string / 2))
    local compartment2 = lh.string_to_set(
                             rucksack_string:sub(#rucksack_string / 2 + 1))
    return {compartment1, compartment2}
end

local function get_common_item(set1, set2, set3)
    for k, _ in pairs(set1) do
        if set3 then
            if set2[k] and set3[k] then return k end
        else
            if set2[k] then return k end
        end
    end
    return nil
end

local function item_value(item)
    local item_byte = string.byte(item)
    if item_byte >= string.byte("a") and item_byte <= string.byte("z") then
        return 1 + item_byte - string.byte("a")
    else
        return 1 + 26 + item_byte - string.byte("A")
    end
end

local function get_rucksack_common_item(rucksack)
    return get_common_item(rucksack[1], rucksack[2])
end

local function solution1(filename)
    local rucksack_strings = read_rucksack_strings(filename)
    local compartmentalized = lh.map(compartmentalize, rucksack_strings)
    local common_items = lh.map(get_rucksack_common_item, compartmentalized)
    local item_values = lh.map(item_value, common_items)
    return lh.sum(item_values)
end

lh.P(solution1("input.txt"))
lh.P(solution1("input2.txt"))

local function group_by(n, list)
    local groups = {}
    local current_group = {}
    for _, e in ipairs(list) do
        table.insert(current_group, e)
        if #current_group == n then
            table.insert(groups, current_group)
            current_group = {}
        end
    end
    return groups
end

local function get_group_common_item(group)
    return get_common_item(group[1], group[2], group[3])
end

local function solution2(filename)
    local rucksack_strings = read_rucksack_strings(filename)
    local rucksack_sets = lh.map(lh.string_to_set, rucksack_strings)
    local grouped = group_by(3, rucksack_sets)
    local common_items = lh.map(get_group_common_item, grouped)
    local item_values = lh.map(item_value, common_items)
    return lh.sum(item_values)
end

lh.P(solution2("input.txt"))
lh.P(solution2("input2.txt"))
