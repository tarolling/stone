// trees in an arena, tuned: nodes are indexes into one vector that every tree of the same depth
// reuses, so building a tree allocates nothing once the vector has grown, and freeing it is
// clearing the vector
const LEAF: u32 = u32::MAX;

struct Arena {
    nodes: Vec<[u32; 2]>,
}

impl Arena {
    fn make(&mut self, depth: i64) -> u32 {
        let children = if depth == 0 {
            [LEAF, LEAF]
        } else {
            [self.make(depth - 1), self.make(depth - 1)]
        };
        self.nodes.push(children);
        (self.nodes.len() - 1) as u32
    }

    fn check(&self, t: u32) -> i64 {
        if t == LEAF {
            return 0;
        }
        let [left, right] = self.nodes[t as usize];
        1 + self.check(left) + self.check(right)
    }
}

fn run(max_depth: i64) {
    let mut arena = Arena { nodes: Vec::new() };
    let stretch = arena.make(max_depth + 1);
    println!("{} {}", max_depth + 1, arena.check(stretch));
    let mut long_lived = Arena { nodes: Vec::new() };
    let root = long_lived.make(max_depth);
    let mut depth = 4;
    while depth <= max_depth {
        let iterations = 1i64 << (max_depth - depth + 4);
        let mut total = 0;
        for _ in 0..iterations {
            arena.nodes.clear();
            let t = arena.make(depth);
            total += arena.check(t);
        }
        println!("{depth} {iterations} {total}");
        depth += 2;
    }
    println!("{} {}", max_depth, long_lived.check(root));
}

fn main() {
    run(std::env::args().nth(1).unwrap().parse().unwrap());
}
