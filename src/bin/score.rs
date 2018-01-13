extern crate santa17;

use santa17::*;

fn main() {
	let g = construct_graph();
	let ps = read_solution(&std::env::args().nth(1).unwrap());
	let mut count = vec![0; N2];
	for i in 0..N1 {
		if ps[i] != !0 {
			count[ps[i]] += 1;
		}
	}
	for j in 0..N2 {
		assert!(count[j] <= 1000);
	}
	let mut err = false;
	for i in 0..K/2 {
		if ps[i * 2] != ps[i * 2 + 1] || ps[i * 2] == !0 {
			err = true;
		}
	}
	if err {
		println!("error!");
	}
	let mut cost = 0;
	for i in 0..N1 {
		for &(j, w) in &g[i] {
			if j == ps[i] {
				cost += w;
			}
		}
	}
	println!("{}", cost_to_score(cost));
	println!("{}", cost);
}
