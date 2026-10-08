-- Coroutine switches: a generator, a pipeline of filters and many short-lived coroutines.

local function numbers(limit)
  return coroutine.wrap(function()
    for i = 1, limit do
      coroutine.yield(i)
    end
  end)
end

local function filter(divisor, source)
  return coroutine.wrap(function()
    for value in source do
      if value % divisor ~= 0 then
        coroutine.yield(value)
      end
    end
  end)
end

local total = 0
for value in numbers(1500000) do
  total = total + value
end
print("generator", total)

local source = numbers(300000)
for _, divisor in ipairs({ 2, 3, 5, 7, 11 }) do
  source = filter(divisor, source)
end
local survivors, last = 0, 0
for value in source do
  survivors = survivors + 1
  last = value
end
print("pipeline", survivors, last)

local finished = 0
for i = 1, 150000 do
  local thread = coroutine.create(function(a, b)
    local c = coroutine.yield(a + b)
    return c * 2
  end)
  local _, first = coroutine.resume(thread, i, 1)
  local _, second = coroutine.resume(thread, first)
  finished = finished + second
end
print("threads", finished)
