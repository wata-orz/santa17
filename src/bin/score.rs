extern crate santa17;

use santa17::*;

fn main() {
	let (g1, g2) = construct_graph();
	let ps = read_solution(&std::env::args().nth(1).unwrap());
	let mut count = vec![0; N];
	let mut err = false;
	for i in 0..N {
		if ps[i] == !0 {
			err = true;
		} else {
			count[ps[i]] += 1;
		}
	}
	if err {
		println!("mismatch!");
	}
	err = false;
	for j in 0..N {
		assert!(count[j] <= 1000);
	}
	for i in 0..N3 {
		if ps[i * 3] != ps[i * 3 + 1] || ps[i * 3] != ps[i * 3 + 2] {
			err = true;
		}
	}
	if err {
		println!("triple!");
	}
	err = false;
	for i in 0..N2 {
		if ps[N3 * 3 + i * 2] != ps[N3 * 3 + i * 2 + 1] {
			err = true;
		}
	}
	if err {
		println!("twin!");
	}
	let mut score1 = 0;
	let mut score2 = 0;
	for i in 0..N {
		for &(j, w) in &g1[i] {
			if j == ps[i] {
				score1 += w;
			}
		}
		for &(j, w) in &g2[i] {
			if j == ps[i] {
				score2 += w;
			}
		}
	}
	println!("{} {}", score1, score2);
	println!("{}", get_score(score1, score2));
}
