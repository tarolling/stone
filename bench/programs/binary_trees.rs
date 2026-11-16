// trees of boxes: building, walking, and freeing many binary trees
struct Node {
    left: Option<Box<Node>>,
    right: Option<Box<Node>>,
}

fn make(depth: i64) -> Box<Node> {
    if depth == 0 {
        return Box::new(Node {
            left: None,
            right: None,
        });
    }
    Box::new(Node {
        left: Some(make(depth - 1)),
        right: Some(make(depth - 1)),
    })
}

fn check(t: &Node) -> i64 {
    let side = |child: &Option<Box<Node>>| child.as_deref().map_or(0, check);
    1 + side(&t.left) + side(&t.right)
}

fn run(max_depth: i64) {
    println!("{} {}", max_depth + 1, check(&make(max_depth + 1)));
    let long_lived = make(max_depth);
    let mut depth = 4;
    while depth <= max_depth {
        let iterations = 1i64 << (max_depth - depth + 4);
        let mut total = 0;
        for _ in 0..iterations {
            total += check(&make(depth));
        }
        println!("{depth} {iterations} {total}");
        depth += 2;
    }
    println!("{} {}", max_depth, check(&long_lived));
}

fn main() {
    run(std::env::args().nth(1).unwrap().parse().unwrap());
}
