-- String building, interning, pattern matching and byte access.

local parts = {}
for i = 1, 200000 do
  parts[#parts + 1] = tostring(i * 7)
end
local joined = table.concat(parts, ",")
print("joined", #joined, joined:sub(1, 16))

local replaced, replacements = joined:gsub("(%d)7", "%1x")
print("gsub", #replaced, replacements)

local words = 0
for _ in joined:gmatch("%d+") do
  words = words + 1
end
print("gmatch", words)

local checksum = 0
for i = 1, #joined do
  checksum = (checksum * 31 + joined:byte(i)) % 1000000007
end
print("bytes", checksum)

local interned = {}
for i = 1, 200000 do
  local key = "key" .. (i % 5000)
  interned[key] = (interned[key] or 0) + #key
end
print("keys", interned.key0, interned.key4999)

local reversed = joined:sub(1, 100000):reverse():upper():rep(4)
print("reversed", #reversed, reversed:find("12,", 1, true))
