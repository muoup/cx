-- Floating-point arithmetic over table fields: the classic n-body simulation.

local sqrt = math.sqrt
local PI = math.pi
local SOLAR_MASS = 4 * PI * PI
local DAYS = 365.24

local bodies = {
  { x = 0, y = 0, z = 0, vx = 0, vy = 0, vz = 0, mass = SOLAR_MASS },
  {
    x = 4.84143144246472090e+00, y = -1.16032004402742839e+00, z = -1.03622044471123109e-01,
    vx = 1.66007664274403694e-03 * DAYS, vy = 7.69901118419740425e-03 * DAYS,
    vz = -6.90460016972063023e-05 * DAYS, mass = 9.54791938424326609e-04 * SOLAR_MASS,
  },
  {
    x = 8.34336671824457987e+00, y = 4.12479856412430479e+00, z = -4.03523417114321381e-01,
    vx = -2.76742510726862411e-03 * DAYS, vy = 4.99852801234917238e-03 * DAYS,
    vz = 2.30417297573763929e-05 * DAYS, mass = 2.85885980666130812e-04 * SOLAR_MASS,
  },
  {
    x = 1.28943695621391310e+01, y = -1.51111514016986312e+01, z = -2.23307578892655734e-01,
    vx = 2.96460137564761618e-03 * DAYS, vy = 2.37847173959480950e-03 * DAYS,
    vz = -2.96589568540237556e-05 * DAYS, mass = 4.36624404335156298e-05 * SOLAR_MASS,
  },
  {
    x = 1.53796971148509165e+01, y = -2.59193146099879641e+01, z = 1.79258772950371181e-01,
    vx = 2.68067772490389322e-03 * DAYS, vy = 1.62824170038242295e-03 * DAYS,
    vz = -9.51592254519715870e-05 * DAYS, mass = 5.15138902046611451e-05 * SOLAR_MASS,
  },
}

local function advance(dt)
  local count = #bodies
  for i = 1, count do
    local a = bodies[i]
    for j = i + 1, count do
      local b = bodies[j]
      local dx, dy, dz = a.x - b.x, a.y - b.y, a.z - b.z
      local distance = sqrt(dx * dx + dy * dy + dz * dz)
      local magnitude = dt / (distance * distance * distance)
      a.vx = a.vx - dx * b.mass * magnitude
      a.vy = a.vy - dy * b.mass * magnitude
      a.vz = a.vz - dz * b.mass * magnitude
      b.vx = b.vx + dx * a.mass * magnitude
      b.vy = b.vy + dy * a.mass * magnitude
      b.vz = b.vz + dz * a.mass * magnitude
    end
  end
  for i = 1, count do
    local body = bodies[i]
    body.x = body.x + dt * body.vx
    body.y = body.y + dt * body.vy
    body.z = body.z + dt * body.vz
  end
end

local function energy()
  local total = 0
  for i = 1, #bodies do
    local a = bodies[i]
    total = total + 0.5 * a.mass * (a.vx * a.vx + a.vy * a.vy + a.vz * a.vz)
    for j = i + 1, #bodies do
      local b = bodies[j]
      local dx, dy, dz = a.x - b.x, a.y - b.y, a.z - b.z
      total = total - a.mass * b.mass / sqrt(dx * dx + dy * dy + dz * dz)
    end
  end
  return total
end

local px, py, pz = 0, 0, 0
for i = 1, #bodies do
  local body = bodies[i]
  px = px + body.vx * body.mass
  py = py + body.vy * body.mass
  pz = pz + body.vz * body.mass
end
bodies[1].vx = -px / SOLAR_MASS
bodies[1].vy = -py / SOLAR_MASS
bodies[1].vz = -pz / SOLAR_MASS

print("before", math.floor(energy() * 1e9))
for _ = 1, 400000 do
  advance(0.01)
end
print("after", math.floor(energy() * 1e9))
