// structs and methods: simulating the orbits of the Sun and the four gas giants
#[derive(Clone, Copy)]
struct Body {
    x: f64,
    y: f64,
    z: f64,
    vx: f64,
    vy: f64,
    vz: f64,
    mass: f64,
}

impl Body {
    fn drift(&mut self, dt: f64) {
        self.x += dt * self.vx;
        self.y += dt * self.vy;
        self.z += dt * self.vz;
    }

    fn kinetic(&self) -> f64 {
        0.5 * self.mass * (self.vx * self.vx + self.vy * self.vy + self.vz * self.vz)
    }
}

fn advance(bodies: &mut [Body], dt: f64, steps: i64) {
    let n = bodies.len();
    for _ in 0..steps {
        for i in 0..n {
            for j in i + 1..n {
                let dx = bodies[i].x - bodies[j].x;
                let dy = bodies[i].y - bodies[j].y;
                let dz = bodies[i].z - bodies[j].z;
                let d2 = dx * dx + dy * dy + dz * dz;
                let mag = dt / (d2 * d2.sqrt());
                let mj = bodies[j].mass * mag;
                bodies[i].vx -= dx * mj;
                bodies[i].vy -= dy * mj;
                bodies[i].vz -= dz * mj;
                let mi = bodies[i].mass * mag;
                bodies[j].vx += dx * mi;
                bodies[j].vy += dy * mi;
                bodies[j].vz += dz * mi;
            }
        }
        for body in bodies.iter_mut() {
            body.drift(dt);
        }
    }
}

fn energy(bodies: &[Body]) -> f64 {
    let mut e = 0.0;
    let n = bodies.len();
    for i in 0..n {
        e += bodies[i].kinetic();
        for j in i + 1..n {
            let dx = bodies[i].x - bodies[j].x;
            let dy = bodies[i].y - bodies[j].y;
            let dz = bodies[i].z - bodies[j].z;
            e -= bodies[i].mass * bodies[j].mass / (dx * dx + dy * dy + dz * dz).sqrt();
        }
    }
    e
}

// sets the Sun moving so the system's total momentum is zero
fn offset_momentum(bodies: &mut [Body], solar_mass: f64) {
    let (mut px, mut py, mut pz) = (0.0, 0.0, 0.0);
    for b in bodies.iter() {
        px += b.vx * b.mass;
        py += b.vy * b.mass;
        pz += b.vz * b.mass;
    }
    bodies[0].vx = -px / solar_mass;
    bodies[0].vy = -py / solar_mass;
    bodies[0].vz = -pz / solar_mass;
}

fn body(x: f64, y: f64, z: f64, vx: f64, vy: f64, vz: f64, mass: f64) -> Body {
    Body {
        x,
        y,
        z,
        vx,
        vy,
        vz,
        mass,
    }
}

fn main() {
    let steps: i64 = std::env::args().nth(1).unwrap().parse().unwrap();
    let pi = 3.141592653589793;
    let solar_mass = 4.0 * pi * pi;
    let days = 365.24;
    // the Sun, then Jupiter, Saturn, Uranus, and Neptune
    let mut bodies = vec![
        body(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, solar_mass),
        body(
            4.84143144246472090e+00,
            -1.16032004402742839e+00,
            -1.03622044471123109e-01,
            1.66007664274403694e-03 * days,
            7.69901118419740425e-03 * days,
            -6.90460016972063023e-05 * days,
            9.54791938424326609e-04 * solar_mass,
        ),
        body(
            8.34336671824457987e+00,
            4.12479856412430479e+00,
            -4.03523417114321381e-01,
            -2.76742510726862411e-03 * days,
            4.99852801234917238e-03 * days,
            2.30417297573763929e-05 * days,
            2.85885980666130812e-04 * solar_mass,
        ),
        body(
            1.28943695621391310e+01,
            -1.51111514016986312e+01,
            -2.23307578892655734e-01,
            2.96460137564761618e-03 * days,
            2.37847173959480950e-03 * days,
            -2.96589568540237556e-05 * days,
            4.36624404335156298e-05 * solar_mass,
        ),
        body(
            1.53796971148509165e+01,
            -2.59193146099879641e+01,
            1.79258772950371181e-01,
            2.68067772490389322e-03 * days,
            1.62824170038242295e-03 * days,
            -9.51592254519715870e-05 * days,
            5.15138902046611451e-05 * solar_mass,
        ),
    ];
    offset_momentum(&mut bodies, solar_mass);
    // energies scaled to ints, so the output need not print floats the way stone does
    println!("{}", (energy(&bodies) * 1000000000.0) as i64);
    advance(&mut bodies, 0.01, steps);
    println!("{}", (energy(&bodies) * 1000000000.0) as i64);
}
