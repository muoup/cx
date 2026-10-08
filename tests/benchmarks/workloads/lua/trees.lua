-- Allocation and garbage collection: the binary-trees benchmark.

local function bottom_up(depth)
  if depth == 0 then return {} end
  return { bottom_up(depth - 1), bottom_up(depth - 1) }
end

local function check(tree)
  if tree[1] then
    return 1 + check(tree[1]) + check(tree[2])
  end
  return 1
end

local MIN_DEPTH, MAX_DEPTH = 4, 15

print("stretch", check(bottom_up(MAX_DEPTH + 1)))

local long_lived = bottom_up(MAX_DEPTH)

for depth = MIN_DEPTH, MAX_DEPTH, 2 do
  local iterations = 1 << (MAX_DEPTH - depth + MIN_DEPTH)
  local nodes = 0
  for _ = 1, iterations do
    nodes = nodes + check(bottom_up(depth))
  end
  print("depth", depth, iterations, nodes)
end

print("long lived", check(long_lived))
