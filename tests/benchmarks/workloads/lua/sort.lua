-- Table reads and writes: sorting with and without a comparator, and hash-part updates.

local state = 12345
local function random()
  state = (state * 1103515245 + 12345) % 2147483648
  return state
end

local values = {}
for i = 1, 400000 do
  values[i] = random() % 1000003
end
table.sort(values)
print("ascending", values[1], values[200000], values[#values])

table.sort(values, function(left, right) return left > right end)
print("descending", values[1], values[200000], values[#values])

local counts = {}
for i = 1, 1500000 do
  local key = random() % 4099
  counts[key] = (counts[key] or 0) + 1
end

local distinct, largest = 0, 0
for _, count in pairs(counts) do
  distinct = distinct + 1
  if count > largest then largest = count end
end
print("counts", distinct, largest)
