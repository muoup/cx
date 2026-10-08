-- Function calls: naive recursion, mutual recursion and varargs.

local function fib(n)
  if n < 2 then return n end
  return fib(n - 1) + fib(n - 2)
end

local is_odd

local function is_even(n)
  if n == 0 then return true end
  return is_odd(n - 1)
end

function is_odd(n)
  if n == 0 then return false end
  return is_even(n - 1)
end

local function sum(...)
  local total = 0
  for i = 1, select("#", ...) do
    total = total + select(i, ...)
  end
  return total
end

print("fib", fib(34))

local evens = 0
for i = 1, 4000 do
  if is_even(i) then evens = evens + 1 end
end
print("evens", evens)

local total = 0
for i = 1, 600000 do
  total = total + sum(i, 2, 3, 4, 5, 6, 7, 8)
end
print("sum", total)
