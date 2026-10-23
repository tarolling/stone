import math
import sys
from dataclasses import dataclass


@dataclass
class Body:
    x: float
    y: float
    z: float
    vx: float
    vy: float
    vz: float
    mass: float

    def drift(self, dt):
        self.x += dt * self.vx
        self.y += dt * self.vy
        self.z += dt * self.vz

    def kinetic(self):
        return (
            0.5
            * self.mass
            * (self.vx * self.vx + self.vy * self.vy + self.vz * self.vz)
        )


def advance(bodies, dt, steps):
    n = len(bodies)
    for _ in range(steps):
        for i in range(n):
            bi = bodies[i]
            for j in range(i + 1, n):
                bj = bodies[j]
                dx = bi.x - bj.x
                dy = bi.y - bj.y
                dz = bi.z - bj.z
                d2 = dx * dx + dy * dy + dz * dz
                mag = dt / (d2 * math.sqrt(d2))
                mj = bj.mass * mag
                bi.vx -= dx * mj
                bi.vy -= dy * mj
                bi.vz -= dz * mj
                mi = bi.mass * mag
                bj.vx += dx * mi
                bj.vy += dy * mi
                bj.vz += dz * mi
        for b in bodies:
            b.drift(dt)


def energy(bodies):
    e = 0.0
    n = len(bodies)
    for i in range(n):
        e += bodies[i].kinetic()
        for j in range(i + 1, n):
            dx = bodies[i].x - bodies[j].x
            dy = bodies[i].y - bodies[j].y
            dz = bodies[i].z - bodies[j].z
            e -= (
                bodies[i].mass * bodies[j].mass / math.sqrt(dx * dx + dy * dy + dz * dz)
            )
    return e


# sets the Sun moving so the system's total momentum is zero
def offset_momentum(bodies, solar_mass):
    px = py = pz = 0.0
    for b in bodies:
        px += b.vx * b.mass
        py += b.vy * b.mass
        pz += b.vz * b.mass
    bodies[0].vx = -px / solar_mass
    bodies[0].vy = -py / solar_mass
    bodies[0].vz = -pz / solar_mass


pi = 3.141592653589793
solar_mass = 4.0 * pi * pi
days = 365.24
bodies = [
    Body(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, solar_mass),
    Body(
        4.84143144246472090e00,
        -1.16032004402742839e00,
        -1.03622044471123109e-01,
        1.66007664274403694e-03 * days,
        7.69901118419740425e-03 * days,
        -6.90460016972063023e-05 * days,
        9.54791938424326609e-04 * solar_mass,
    ),
    Body(
        8.34336671824457987e00,
        4.12479856412430479e00,
        -4.03523417114321381e-01,
        -2.76742510726862411e-03 * days,
        4.99852801234917238e-03 * days,
        2.30417297573763929e-05 * days,
        2.85885980666130812e-04 * solar_mass,
    ),
    Body(
        1.28943695621391310e01,
        -1.51111514016986312e01,
        -2.23307578892655734e-01,
        2.96460137564761618e-03 * days,
        2.37847173959480950e-03 * days,
        -2.96589568540237556e-05 * days,
        4.36624404335156298e-05 * solar_mass,
    ),
    Body(
        1.53796971148509165e01,
        -2.59193146099879641e01,
        1.79258772950371181e-01,
        2.68067772490389322e-03 * days,
        1.62824170038242295e-03 * days,
        -9.51592254519715870e-05 * days,
        5.15138902046611451e-05 * solar_mass,
    ),
]
offset_momentum(bodies, solar_mass)
# energies scaled to ints, so C need not print floats the way stone does
print(int(energy(bodies) * 1000000000.0))
advance(bodies, 0.01, int(sys.argv[1]))
print(int(energy(bodies) * 1000000000.0))
