local lh = dofile("../lua/help.lua")

local rock1 = "A"
local rock2 = "X"
local paper1 = "B"
local paper2 = "Y"
local scissor1 = "C"
local scissor2 = "Z"

local function match_score(match)
    local score = 0
    if match[2] == rock2 then
        score = score + 1
    elseif match[2] == paper2 then
        score = score + 2
    else
        score = score + 3
    end

    if (match[1] == rock1 and match[2] == paper2) or
        (match[1] == paper1 and match[2] == scissor2) or
        (match[1] == scissor1 and match[2] == rock2) then
        score = score + 6
    elseif (match[1] == rock1 and match[2] == rock2) or
        (match[1] == paper1 and match[2] == paper2) or
        (match[1] == scissor1 and match[2] == scissor2) then
        score = score + 3
    else
        score = score + 0
    end

    return score
end

local function total_score(lists)
    local score_sum = 0
    for _, match in ipairs(lists) do
        score_sum = score_sum + match_score(match)
    end
    return score_sum
end

lh.P(total_score(lh.read_lists_same_line("input.txt")))

