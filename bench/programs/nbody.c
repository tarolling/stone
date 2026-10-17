#include <math.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct {
    double x, y, z, vx, vy, vz, mass;
} Body;

#define N 5

static void drift(Body *b, double dt) {
    b->x += dt * b->vx;
    b->y += dt * b->vy;
    b->z += dt * b->vz;
}

static double kinetic(const Body *b) {
    return 0.5 * b->mass * (b->vx * b->vx + b->vy * b->vy + b->vz * b->vz);
}

void advance(Body *bodies, double dt, long steps) {
    for (long s = 0; s < steps; s++) {
        for (int i = 0; i < N; i++) {
            for (int j = i + 1; j < N; j++) {
                double dx = bodies[i].x - bodies[j].x;
                double dy = bodies[i].y - bodies[j].y;
                double dz = bodies[i].z - bodies[j].z;
                double d2 = dx * dx + dy * dy + dz * dz;
                double mag = dt / (d2 * sqrt(d2));
                double mj = bodies[j].mass * mag;
                bodies[i].vx -= dx * mj;
                bodies[i].vy -= dy * mj;
                bodies[i].vz -= dz * mj;
                double mi = bodies[i].mass * mag;
                bodies[j].vx += dx * mi;
                bodies[j].vy += dy * mi;
                bodies[j].vz += dz * mi;
            }
        }
        for (int i = 0; i < N; i++)
            drift(&bodies[i], dt);
    }
}

double energy(const Body *bodies) {
    double e = 0.0;
    for (int i = 0; i < N; i++) {
        e += kinetic(&bodies[i]);
        for (int j = i + 1; j < N; j++) {
            double dx = bodies[i].x - bodies[j].x;
            double dy = bodies[i].y - bodies[j].y;
            double dz = bodies[i].z - bodies[j].z;
            e -= bodies[i].mass * bodies[j].mass / sqrt(dx * dx + dy * dy + dz * dz);
        }
    }
    return e;
}

// sets the Sun moving so the system's total momentum is zero
static void offset_momentum(Body *bodies, double solar_mass) {
    double px = 0.0, py = 0.0, pz = 0.0;
    for (int i = 0; i < N; i++) {
        px += bodies[i].vx * bodies[i].mass;
        py += bodies[i].vy * bodies[i].mass;
        pz += bodies[i].vz * bodies[i].mass;
    }
    bodies[0].vx = -px / solar_mass;
    bodies[0].vy = -py / solar_mass;
    bodies[0].vz = -pz / solar_mass;
}

int main(int argc, char **argv) {
    double pi = 3.141592653589793;
    double solar_mass = 4.0 * pi * pi;
    double days = 365.24;
    Body bodies[N] = {
        {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, solar_mass},
        {4.84143144246472090e+00, -1.16032004402742839e+00, -1.03622044471123109e-01,
         1.66007664274403694e-03 * days, 7.69901118419740425e-03 * days,
         -6.90460016972063023e-05 * days, 9.54791938424326609e-04 * solar_mass},
        {8.34336671824457987e+00, 4.12479856412430479e+00, -4.03523417114321381e-01,
         -2.76742510726862411e-03 * days, 4.99852801234917238e-03 * days,
         2.30417297573763929e-05 * days, 2.85885980666130812e-04 * solar_mass},
        {1.28943695621391310e+01, -1.51111514016986312e+01, -2.23307578892655734e-01,
         2.96460137564761618e-03 * days, 2.37847173959480950e-03 * days,
         -2.96589568540237556e-05 * days, 4.36624404335156298e-05 * solar_mass},
        {1.53796971148509165e+01, -2.59193146099879641e+01, 1.79258772950371181e-01,
         2.68067772490389322e-03 * days, 1.62824170038242295e-03 * days,
         -9.51592254519715870e-05 * days, 5.15138902046611451e-05 * solar_mass},
    };
    offset_momentum(bodies, solar_mass);
    printf("%ld\n", (long)(energy(bodies) * 1000000000.0));
    advance(bodies, 0.01, atol(argv[1]));
    printf("%ld\n", (long)(energy(bodies) * 1000000000.0));
    return 0;
}
