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

local function total_score(matches)
    local score_sum = 0
    for _, match in ipairs(matches) do
        score_sum = score_sum + match_score(match)
    end
    return score_sum
end

local loss = "X"
local draw = "Y"
local win = "Z"

local function transform_result_to_choice(matches)
    local transformed_matches = {}
    for _, match in ipairs(matches) do
        local new_match = {match[1]}
        if match[2] == loss then
            if match[1] == rock1 then
                table.insert(new_match, scissor2)
            elseif match[1] == paper1 then
                table.insert(new_match, rock2)
            else
                table.insert(new_match, paper2)
            end
        elseif match[2] == draw then
            if match[1] == rock1 then
                table.insert(new_match, rock2)
            elseif match[1] == paper1 then
                table.insert(new_match, paper2)
            else
                table.insert(new_match, scissor2)
            end
        else -- win
            if match[1] == rock1 then
                table.insert(new_match, paper2)
            elseif match[1] == paper1 then
                table.insert(new_match, scissor2)
            else
                table.insert(new_match, rock2)
            end
        end
        table.insert(transformed_matches, new_match)
    end
    return transformed_matches
end

lh.P(total_score(lh.read_lists_same_line("input.txt")))

lh.P(total_score(
         transform_result_to_choice(lh.read_lists_same_line("input.txt"))))

