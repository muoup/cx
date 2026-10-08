-- Closures, upvalues and metatable dispatch: method calls, inheritance and operators.

local Shape = {}
Shape.__index = Shape

function Shape.new(width, height)
  return setmetatable({ width = width, height = height }, Shape)
end

function Shape:area() return self.width * self.height end
function Shape:scale(factor)
  self.width = self.width * factor
  self.height = self.height * factor
end

local Square = setmetatable({}, { __index = Shape })
Square.__index = Square

function Square.new(side)
  return setmetatable(Shape.new(side, side), Square)
end

function Square:area() return self.width * self.width end

local Vector = {}
Vector.__index = Vector
Vector.__add = function(a, b) return setmetatable({ a[1] + b[1], a[2] + b[2] }, Vector) end
Vector.__eq = function(a, b) return a[1] == b[1] and a[2] == b[2] end

local shapes = {}
for i = 1, 1000 do
  shapes[i] = i % 3 == 0 and Square.new(i % 17 + 1) or Shape.new(i % 13 + 1, i % 7 + 1)
end

local area = 0
for round = 1, 1500 do
  for i = 1, #shapes do
    area = area + shapes[i]:area()
  end
  if round % 500 == 0 then
    for i = 1, #shapes do shapes[i]:scale(2) end
  end
end
print("area", area)

local function counter(step)
  local count = 0
  return function()
    count = count + step
    return count
  end
end

local counters = {}
for i = 1, 100 do counters[i] = counter(i) end
local counted = 0
for _ = 1, 20000 do
  for i = 1, #counters do
    counted = counted + counters[i]()
  end
end
print("counters", counted)

local position, step = setmetatable({ 0, 0 }, Vector), setmetatable({ 1, 2 }, Vector)
local equal = 0
for i = 1, 1000000 do
  position = position + step
  if position == step then equal = equal + 1 end
end
print("vector", position[1], position[2], equal)
