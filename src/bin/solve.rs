extern crate santa17;
extern crate rand;

use santa17::*;
use santa17::common::*;
use rand::Rng;

fn init(g: &Vec<Vec<(usize, i64)>>, g2: &Vec<Vec<(usize, i64)>>) -> Vec<usize> {
	if std::fs::File::open("ub.csv").is_ok() {
		return read_solution("ub.csv");
	}
	let mut ps = vec![!0; N1];
	let mut count = vec![0; N2];
	let mut mcf = mincostcirculation::Graph::new(N1 + N2 + 1);
	let r = N1 + N2;
	for i in 0..K {
		if i / 2 == 1802 { continue }//
		mcf.add(r, i, 1, 0);
		for &(j, w) in &g2[i / 2] {
			mcf.add(i, N1 + j, 1, w);
		}
	}
	for i in K..N1 {
		mcf.add(r, i, 1, 0);
		for &(j, w) in &g[i] {
			mcf.add(i, N1 + j, 1, w);
		}
	}
	for j in 0..N2 {
		mcf.add(N1 + j, r, 1000, 0);
	}
	mcf.solve();
	for i in 0..N1 {
		for &e in &mcf.es[i] {
			if N1 <= e.to && e.to < r && e.cap == 0 {
				let j = e.to - N1;
				ps[i] = j;
				count[j] += 1;
			}
		}
	}
	write_solution(&ps, "ub.csv");
	ps
}

fn main() {
	let g = construct_graph();
	let g2: Vec<Vec<_>> = (0..K/2).map(|i| merge(&g[i * 2], &g[i * 2 + 1]).into_iter().map(|(j, w)| (j, w / 2)).collect()).collect();
	let mut ps = init(&g, &g2);
	let mut cost = 0;
	for i in 0..K {
		cost += get_cost(&g2[i / 2], ps[i]).unwrap_or_default();
	}
	for i in K..N1 {
		cost += get_cost(&g[i], ps[i]).unwrap_or_default();
	}
	eprintln!("{}", cost);
	eprintln!("{:.8}", cost_to_score(cost));
	for i in 0..K/2 {
		if ps[i * 2] != ps[i * 2 + 1] {
			eprintln!("mismatch: {} {:?} {:?}", i, get_cost(&g2[i], ps[i * 2]), get_cost(&g2[i], ps[i * 2 + 1]));
			let i2 = if ps[i * 2] != 0 {
				i * 2
			} else if ps[i * 2 + 1] != 0 {
				i * 2 + 1
			} else {
				!0
			};
			if i2 != !0 {
				let mut t = !0;
				let mut max = get_cost(&g2[i], ps[i2]).unwrap();
				for k in K..N1 {
					if ps[k] == ps[i2] && max.setmax(get_cost(&g[k], ps[k]).unwrap()) {
						t = k;
					}
				}
				if t != !0 {
					eprintln!("{}", max);
					ps[t] = !0;
					let j = ps[i2];
					ps[i * 2] = j;
					ps[i * 2 + 1] = j;
				} else {
					ps[i * 2] = !0;
					ps[i * 2 + 1] = !0;
				}
			} else {
				ps[i * 2] = !0;
				ps[i * 2 + 1] = !0;
			}
		}
	}
	let mut count = vec![0; N2];
	let mut cost = 0;
	for i in 0..N1 {
		if ps[i] != !0 {
			count[ps[i]] += 1;
			cost += get_cost(&g[i], ps[i]).unwrap_or_default();
		}
	}
	eprintln!("{:.8}", cost_to_score(cost));
	let mut n1 = 0;
	for i in 0..K/2 {
		if ps[i * 2] == !0 {
			n1 += 1;
		}
	}
	let mut n2 = 0;
	for j in 0..N2 {
		n2 += (1000 - count[j]) / 2;
	}
	eprintln!("{} {}", n1, n2);
	let mut min = 0;
	let mut rng = rand::XorShiftRng::new_unseeded();
	loop {
		let mut mcf = mincostflow::Graph::new(N1 + N2 + 4);
		let r = N1 + N2;
		let s = r + 1;
		let t = r + 2;
		let t2 = r + 3;
		for i in K..N1 {
			if ps[i] == !0 {
				mcf.add(r, i, 1, 0);
			} else {
				mcf.add(i, r, 1, 0);
			}
			for &(j, w) in &g[i] {
				if j != ps[i] {
					mcf.add(i, N1 + j, 1, w);
				} else {
					mcf.add(N1 + j, i, 1, -w);
				}
			}
		}
		for j in 0..N2 {
			if count[j] % 2 == 1 {
				if rng.gen() {
					mcf.add(s, N1 + j, 1, 0);
				} else {
					mcf.add(N1 + j, t, 1, 0);
				}
			}
		}
		mcf.add(t, t2, n1 - n2, 0);
		if mcf.solve(s, t2, true).0 != n1 - n2 {
			eprintln!("failed");
			continue;
		}
		let mut ps = ps.clone();
		for i in K..N1 {
			ps[i] = !0;
			for &e in &mcf.es[i] {
				if N1 <= e.to && e.to < r && e.cap == 0 {
					let j = e.to - N1;
					ps[i] = j;
				}
			}
		}
		let mut count = vec![0; N2];
		let mut cost = 0;
		for i in 0..N1 {
			if ps[i] != !0 {
				count[ps[i]] += 1;
				cost += get_cost(&g[i], ps[i]).unwrap_or_default();
			}
		}
		let mut n1 = 0;
		for i in 0..K/2 {
			if ps[i * 2] == !0 {
				n1 += 1;
			}
		}
		let mut n2 = 0;
		for j in 0..N2 {
			n2 += (1000 - count[j]) / 2;
		}
		eprintln!("{} {}", n1, n2);
		assert!(n1 <= n2);
		eprintln!("{:.8}", cost_to_score(cost));
		if !min.setmin(cost) {
			continue;
		}
		eprintln!("updated: {}", cost);
		for i in 0..K/2 {
			if ps[i * 2] == !0 {
				for j in 0..N2 {
					if count[j] <= 998 {
						ps[i * 2] = j;
						ps[i * 2 + 1] = j;
						count[j] += 2;
						break;
					}
				}
			}
		}
		for i in 0..N1 {
			if ps[i] == !0 {
				for j in 0..N2 {
					if count[j] < 1000 {
						ps[i] = j;
						count[j] += 1;
						break;
					}
				}
			}
		}
		write_solution(&ps, "out.csv");
	}
}
