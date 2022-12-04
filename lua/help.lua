local M = {}

function M.P(any, sameline)
    if vim then
        print(vim.inspect(any))
    else
        if type(any) == "table" then
            io.write('{ ')
            for k, v in pairs(any) do
                io.write(k, " = ")
                M.P(v, true)
                io.write(", ")
            end
            io.write('}')
        else
            io.write(tostring(any))
        end
        if not sameline then print() end
    end
end

function M.sum(list)
    local s = 0
    for _, e in ipairs(list) do s = s + e end
    return s
end

function M.map(fn, list)
    local result = {}
    for _, e in ipairs(list) do
        local r = fn(e)
        table.insert(result, r)
    end
    return result
end

function M.split_string_sep(separator)
    return function(str) return M.split_string(str, separator) end
end

function M.split_string(str, separator)
    if separator == nil then separator = "%s" end
    local list = {}
    for token in string.gmatch(str, "[^" .. separator .. "]+") do
        table.insert(list, token)
    end
    return list
end

function M.string_to_set(str)
    local set = {}
    for i = 1, #str do set[str:sub(i, i)] = true end
    return set
end

function M.string_to_list(str)
    local list = {}
    for i = 1, #str do table.insert(list, str:sub(i, i)) end
    return list
end

function M.read_list(name)
    local list = {}
    for line in io.lines(name) do table.insert(list, line) end
    return list
end

function M.read_lists_same_line(name)
    local list = {}
    for line in io.lines(name) do table.insert(list, M.split_string(line)) end
    return list
end

function M.read_nested_lists(name)
    local list = {}
    local nested_list = {}
    for line in io.lines(name) do
        local line_is_empty = line == ""
        if line_is_empty then
            table.insert(list, nested_list)
            nested_list = {}
        else
            table.insert(nested_list, line)
        end
    end
    return list
end

function M.contains(A, B)
    for k in pairs(B) do if A[k] == nil then return false end end
    return true
end

function M.count(fn, list)
    local sum = 0
    for _, e in ipairs(list) do if fn(e) then sum = sum + 1 end end
    return sum
end

return M
