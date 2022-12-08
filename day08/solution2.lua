local lh = dofile "../lua/help.lua"

local function scenic_score_of_tree(trees, i, j)
    local tree_height = trees[i][j]
    local right_score, left_score, up_score, down_score = 0, 0, 0, 0
    for k = i + 1, #trees do
        right_score = right_score + 1
        if trees[k][j] >= tree_height then break end
    end
    for k = i - 1, 1, -1 do
        left_score = left_score + 1
        if trees[k][j] >= tree_height then break end
    end
    for k = j + 1, #trees do
        down_score = down_score + 1
        if trees[i][k] >= tree_height then break end
    end
    for k = j - 1, 1, -1 do
        up_score = up_score + 1
        if trees[i][k] >= tree_height then break end
    end
    return right_score * left_score * up_score * down_score
end

local function solution2(trees)
    local highest_scenic_score = 0

    for i = 1, #trees do
        for j = 1, #trees do
            local score = scenic_score_of_tree(trees, i, j)
            if score > highest_scenic_score then
                highest_scenic_score = score
            end
        end
    end

    print(highest_scenic_score)
end

solution2(lh.read_number_matrix("input2.txt"))
