local lh = dofile "../lua/help.lua"

local function assignment_str_to_set(assignment_str)
    local sections = lh.split_string(assignment_str, "-")
    local set = {}
    for i = sections[1], sections[2] do set[i] = true end
    return set
end

local function assignment_pair_is_invalid(assignment_pair)
    local sets = lh.map(assignment_str_to_set, assignment_pair)
    return lh.is_superset(sets[1], sets[2]) or lh.is_superset(sets[2], sets[1])
end

local function solution1(filename)
    local list = lh.read_list(filename)
    local assignment_string_pairs = lh.map(lh.split_string_sep(","), list)
    local intersections = lh.map(assignment_pair_is_invalid,
                                 assignment_string_pairs)
    local number_of_invalid = lh.count(function(e) return e == true end,
                                       intersections)
    lh.P(number_of_invalid)
end

solution1("input.txt")
solution1("input2.txt")

local function assignments_intersects(assignment_pair)
    local sets = lh.map(assignment_str_to_set, assignment_pair)
    local intersection = lh.intersection(sets[1], sets[2])
    return lh.keys_in_table(intersection) > 0
end

local function solution2(filename)
    local list = lh.read_list(filename)
    local assignment_string_pairs = lh.map(lh.split_string_sep(","), list)
    local intersections =
        lh.map(assignments_intersects, assignment_string_pairs)
    local number_of_intersections = lh.count(function(e) return e == true end,
                                             intersections)
    lh.P(number_of_intersections)
end

solution2("input2.txt")
