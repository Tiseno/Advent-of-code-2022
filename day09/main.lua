local lh = dofile "../lua/help.lua"

local function print_state(state, head, tail)
    for i = 1, #state do
        for j = 1, #state[i] do
            if i == head.x and j == head.y then
                io.write("H")
            elseif i == tail.x and j == tail.y then
                io.write("T")
            elseif state[i][j] then
                io.write("#")
            else
                io.write(".")
            end
        end
        print()
    end
end

local function move(direction, head, tail)
    if direction == "U" then
        if tail.x < head.x then
            tail.x = tail.x + 1
            if tail.y ~= head.y then tail.y = head.y end
        end
        return {x = head.x + 1, y = head.y}, tail
    end
    if direction == "D" then
        if tail.x > head.x then
            tail.x = tail.x - 1
            if tail.y ~= head.y then tail.y = head.y end
        end
        return {x = head.x - 1, y = head.y}, tail
    end
    if direction == "L" then
        if tail.y > head.y then
            tail.y = tail.y - 1
            if tail.x ~= head.x then tail.x = head.x end
        end
        return {x = head.x, y = head.y - 1}, tail
    end
    if direction == "R" then
        if tail.y < head.y then
            tail.y = tail.y + 1
            if tail.x ~= head.x then tail.x = head.x end
        end
        return {x = head.x, y = head.y + 1}, tail
    end
    error "Unrecognized direction"
end

local function solution1(motions)
    local head = {x = 1, y = 1} -- visited_state[x][y]
    local tail = {x = 1, y = 1}
    local visited_state = {{true}}
    local iteration = 1
    for _, motion in ipairs(motions) do
        local direction = motion[1]
        for i = 1, motion[2] do
            local new_head, new_tail = move(direction, head, tail)
            -- we visit a new row
            if new_head.x > #visited_state then
                local new_row = lh.create_list(new_head.y, false)
                table.insert(visited_state, new_row)
            end
            -- we visit a new negative row position
            -- insert new row at beginning
            if new_head.x < 1 then
                local new_row = lh.create_list(new_head.y, false)
                table.insert(visited_state, 1, new_row)
                new_head.x = 1
                new_tail.x = new_tail.x + 1
            end
            -- we visit a new column in an existing row
            while new_head.y > #visited_state[new_head.x] do
                table.insert(visited_state[new_head.x], false)
            end
            -- we visit a new negative column position
            -- shift all lists
            if new_head.y < 1 then
                for x = 1, #visited_state do
                    table.insert(visited_state[x], 1, false)
                end
                new_head.y = 1
                new_tail.y = new_tail.y + 1
            end
            head = new_head
            tail = new_tail
            visited_state[tail.x][tail.y] = true
            -- make it interactive
            -- print_state(visited_state, head, tail)
            -- io.read()
            iteration = iteration + 1
        end
    end

    print_state(visited_state, head, tail)

    lh.P(lh.sum(lh.map(function(row)
        return lh.count(function(value) return value end, row)
    end, visited_state)))
end

solution1(lh.read_lists_same_line("input.txt"))

