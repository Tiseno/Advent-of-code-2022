local lh = dofile "../lua/help.lua"

local function assignment_str_to_set(assignment_str)
    local sections = lh.split_string(assignment_str, "-")
    local set = {}
    for i = sections[1], sections[2] do set[i] = true end
    return set
end

local function assignments_intersects(assignment_pair)
    local sets = lh.map(assignment_str_to_set, assignment_pair)
    return lh.contains(sets[1], sets[2]) or lh.contains(sets[2], sets[1])
end

local function solution1(filename)
    local list = lh.read_list(filename)
    local assignment_string_pairs = lh.map(lh.split_string_sep(","), list)
    local intersections =
        lh.map(assignments_intersects, assignment_string_pairs)
    local number_of_intersections = lh.count(function(e) return e == true end,
                                             intersections)
    lh.P(number_of_intersections)
end

solution1("input.txt")
solution1("input2.txt")
