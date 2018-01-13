#![feature(i128_type)]

extern crate santa17;

use santa17::*;

fn modify(ps: &Vec<usize>) -> Vec<usize> {
	let mut qs = vec![];
	for i in 0..N3 {
		for _ in 0..3 {
			qs.push(ps[i]);
		}
	}
	for i in 0..N2 {
		for _ in 0..2 {
			qs.push(ps[N3 + i]);
		}
	}
	for i in N3 + N2..ps.len() {
		qs.push(ps[i]);
	}
	qs
}

fn packing(mut ps: Vec<usize>, mul: &Vec<i32>, cap: &Vec<i32>) -> Option<Vec<usize>> {
	let n = mul.len();
	let m = cap.len();
	let mut count = vec![0; m];
	let mut nums = vec![vec![]; 4];
	for i in 0..n {
		if ps[i] != !0 {
			count[ps[i]] += mul[i];
		} else {
			nums[mul[i] as usize].push(i);
		}
	}
	let mut rem = vec![];
	for i in 0..m {
		if count[i] < cap[i] {
			rem.push(i);
		}
	}
	let n2 = nums[2].len();
	let n3 = nums[3].len();
	eprintln!("{} {} {}", rem.len(), n2, n3);
	let mut dp = mat![!0; rem.len() + 1; n2 + 1; n3 + 1];
	dp[0][0][0] = !1;
	for i in 0..rem.len() {
		for a in 0..n2+1 {
			for b in 0..n3+1 {
				if dp[i][a][b] != !0 {
					for j in 0..n2-a+1 {
						if j as i32 * 2 <= cap[rem[i]] - count[rem[i]] && (cap[rem[i]] - count[rem[i]] - j as i32 * 2) % 3 == 0 && b as i32 + (cap[rem[i]] - count[rem[i]] - j as i32 * 2) / 3 <= n3 as i32 {
							let a2 = a + j;
							let b2 = b as i32 + (cap[rem[i]] - count[rem[i]] - j as i32 * 2) / 3;
							dp[i + 1][a2][b2 as usize] = j;
						}
					}
				}
			}
		}
	}
	if dp[rem.len()][n2][n3] == !0 {
		return None;
	}
	let mut a = n2;
	let mut b = n3;
	for i in (0..rem.len()).rev() {
		let j2 = dp[i + 1][a][b];
		let j3 = (cap[rem[i]] - count[rem[i]] - j2 as i32 * 2) / 3;
		for _ in 0..j2 {
			let x = nums[2].pop().unwrap();
			ps[x] = rem[i];
		}
		for _ in 0..j3 {
			let x = nums[3].pop().unwrap();
			ps[x] = rem[i];
		}
		a -= j2;
		b -= j3 as usize;
	}
	for i in 0..n {
		if ps[i] != !0 { continue }
		assert!(mul[i] == 1);
		for j in 0..m {
			if count[j] < cap[j] {
				ps[i] = j;
				count[j] += 1;
				break;
			}
		}
		assert!(ps[i] != !0);
	}
	Some(ps)
}

fn choose(data: &Data, g: &mut mincostcirculation::Graph<i32, i128>) -> ((usize, usize), (i128, i128)) {
	let n = data.mul.len();
	let m = data.cap.len();
	let r = n + m;
	let mut scores = (data.score + 1, data.score + 1);
	let mut score = data.score;
	let mut t = (!0, !0);
	for i in 0..n {
		if data.mul[i] == 1 { continue }
		for j in 0..g.es[i].len() {
			let e = g.es[i][j];
			if n <= e.to && e.to < r && 0 < e.cap && e.cap < data.mul[i] {
				eprintln!("try: {} -> {}", i, e.to - n);
				let mut tmp = (0, 0);
				for _ in 0..data.mul[i] {
					score -= e.cost;
					score -= g.dec(r, i);
					score -= g.dec(e.to, r);
				}
				tmp.0 = score;
				for _ in 0..data.mul[i] {
					score += e.cost;
					score -= g.dec(i, e.to);
					score -= g.inc(r, i);
					score -= g.inc(e.to, r);
				}
				tmp.1 = score;
				for _ in 0..data.mul[i] {
					score -= g.inc(i, e.to);
				}
				eprintln!("{:?}", tmp);
				if tmp.0 <= data.lb || tmp.1 <= data.lb {
					scores = tmp;
					t = (i, e.to - n);
					break;
				} else if scores.0.max(scores.1) > tmp.0.max(tmp.1)
						|| scores.0.max(scores.1) == tmp.0.max(tmp.1) && scores.0 + scores.1 > tmp.0 + tmp.1 {
					scores = tmp;
					t = (i, e.to - n);
				}
			}
		}
	}
	(t, scores)
}

fn rec(data: &mut Data, g: &mut mincostcirculation::Graph<i32, i128>, dir: &str) {
	let n = data.mul.len();
	let m = data.cap.len();
	let r = n + m;
	eprintln!("{}: score: {} ({})", dir, data.score, data.lb);
	let ((i, j), scores) = choose(data, g);
	if i != !0 {
		eprintln!("{:?}", scores);
		if scores.0 <= data.lb && scores.1 <= data.lb {
			return;
		}
		let cost = get_cost(&data.a[i], j).unwrap();
		if scores.0 >= scores.1 {
			if scores.1 <= data.lb {
				eprintln!("{}: reduce: {} = {}", dir, i, j);
			} else {
				eprintln!("{}: branch: {} = {}", dir, i, j);
			}
			for _ in 0..data.mul[i] {
				data.score += cost;
				data.fixed[data.id[i]] = j;
				data.score -= g.dec(r, i);
				data.score -= g.dec(n + j, r);
			}
			assert_eq!(data.score, scores.0);
			if data.score > data.lb {
				let dir2 = if scores.1 <= data.lb { dir.to_string() } else { dir.to_string() + "+" };
				rec(data, g, &dir2);
			}
			if scores.1 > data.lb {
				eprintln!("{}: branch: {} != {}", dir, i, j);
				for _ in 0..data.mul[i] {
					data.score -= cost;
					data.fixed[data.id[i]] = !0;
					data.score -= g.dec(i, n + j);
					data.score -= g.inc(r, i);
					data.score -= g.inc(n + j, r);
				}
				assert_eq!(data.score, scores.1);
				if data.score > data.lb {
					rec(data, g, &(dir.to_string() + "-"));
				}
				for _ in 0..data.mul[i] {
					data.score -= g.inc(i, n + j);
				}
			} else {
				for _ in 0..data.mul[i] {
					data.score -= cost;
					data.fixed[data.id[i]] = !0;
					data.score -= g.inc(r, i);
					data.score -= g.inc(n + j, r);
				}
			}
		} else {
			if scores.0 <= data.lb {
				eprintln!("{}: reduce: {} != {}", dir, i, j);
			} else {
				eprintln!("{}: branch: {} != {}", dir, j, j);
			}
			for _ in 0..data.mul[i] {
				data.score -= g.dec(i, n + j);
			}
			assert_eq!(data.score, scores.1);
			if data.score > data.lb {
				let dir2 = if scores.0 <= data.lb { dir.to_string() } else { dir.to_string() + "+" };
				rec(data, g, &dir2);
			}
			if scores.0 > data.lb {
				eprintln!("{}: branch: {} = {}", dir, i, j);
				for _ in 0..data.mul[i] {
					data.score += cost;
					data.fixed[i] = j;
					data.score -= g.dec(r, i);
					data.score -= g.dec(n + j, r);
				}
				assert_eq!(data.score, scores.0);
				if data.score > data.lb {
					rec(data, g, &(dir.to_string() + "-"));
				}
				for _ in 0..data.mul[i] {
					data.score -= cost;
					data.score -= g.inc(i, n + j);
					data.score -= g.inc(r, i);
					data.score -= g.inc(n + j, r);
				}
			} else {
				for _ in 0..data.mul[i] {
					data.score -= g.inc(i, n + j);
				}
			}
		}
	} else {
		let mut ps = data.fixed.clone();
		for i in 0..n {
			for e in &g.es[i] {
				if n <= e.to && e.to < r && g.es[e.to][e.rev].cap > 0 {
					ps[data.id[i]] = e.to - n;
				}
			}
		}
		let mut sub = vec![!0; n];
		for i in 0..n {
			sub[i] = ps[data.id[i]];
		}
		if let Some(qs) = packing(sub, &data.mul, &data.cap) {
			eprintln!("{}: update: {}", dir, data.score);
			data.lb = data.score;
			for i in 0..n {
				ps[data.id[i]] = qs[i];
			}
			write_solution(&modify(&ps), "out.csv");
		} else {
			eprintln!("{}: packing failed: {}", dir, data.score);
		}
	}
}

struct Data {
	fixed: Vec<usize>,
	id: Vec<usize>,
	score: i128,
	lb: i128,
	a: Vec<Vec<(usize, i128)>>,
	mul: Vec<i32>,
	cap: Vec<i32>,
}

fn reduce(data: &mut Data) {
	let n = data.mul.len();
	let m = data.cap.len();
	let g = solve_relax(data);
	let r = n + m;
	let ub = data.score - g.val::<i128>();
	eprintln!("ub = {}", ub);
	let mut es = vec![vec![]; n + m + 1];
	for i in 0..n + m + 1 {
		for e in &g.es[i] {
			if e.cap > 0 && -ub + e.cost + g.p[i] - g.p[e.to] < -data.lb {
				es[e.to].push((i, e.cost + g.p[i] - g.p[e.to]));
			}
		}
	}
	let mut fixed = vec![!0; n];
	for j in 0..m {
		let s = n + j;
		let mut dist = vec![None; n + m + 1];
		let mut que = ::std::collections::BinaryHeap::new();
		dist[s] = Some(0);
		que.push((s, 0));
		while let Some((u, d)) = que.pop() {
			let d = -d;
			if dist[u].unwrap() < d { continue }
			for &(v, cost) in &es[u] {
				let d2 = d + cost;
				if dist[v] == None || dist[v].unwrap() > d2 {
					dist[v] = Some(d2);
					que.push((v, -d2));
				}
			}
		}
		for e in &g.es[n + j] {
			if e.to < n && e.cap > 0 {
				if dist[e.to] == None || ub - e.cost - g.p[n + j] + g.p[e.to] - dist[e.to].unwrap() <= data.lb {
					assert!(fixed[e.to] == !0);
					fixed[e.to] = j;
				}
			}
		}
	}
	let mut cap2 = data.cap.clone();
	let mut id2 = vec![];
	let mut a2 = vec![];
	let mut mul2 = vec![];
	for i in 0..n {
		if fixed[i] == !0 {
			id2.push(data.id[i]);
			let mut tmp = vec![];
			for e in &g.es[i] {
				if n <= e.to && e.to < r {
					if e.cap > 0 && -ub + e.cost + g.p[i] - g.p[e.to] >= -data.lb {
						continue;
					}
					tmp.push((e.to - n, -e.cost));
				}
			}
			a2.push(tmp);
			mul2.push(data.mul[i]);
		} else {
			data.fixed[data.id[i]] = fixed[i];
			cap2[fixed[i]] -= data.mul[i];
			data.score += get_cost(&data.a[i], fixed[i]).unwrap() * data.mul[i] as i128;
		}
	}
	eprintln!("reduced: {} -> {}", n, id2.len());
	data.id = id2;
	data.a = a2;
	data.mul = mul2;
	data.cap = cap2;
}

fn solve_relax(data: &Data) -> mincostcirculation::Graph<i32, i128> {
	let n = data.mul.len();
	let m = data.cap.len();
	eprintln!("solving the relaxed problem...");
	let mut g = mincostcirculation::Graph::new(n + m + 1);
	let r = n + m;
	for i in 0..n {
		g.add(r, i, data.mul[i], 0);
		for &(j, w) in &data.a[i] {
			g.add(i, n + j, data.mul[i], -w);
		}
	}
	for j in 0..m {
		g.add(n + j, r, data.cap[j], 0);
	}
	g.solve();
	g.fitting();
	g.check_potential();
	g
}

pub fn solve(a: Vec<Vec<(usize, i128)>>, mul: Vec<i32>, cap: Vec<i32>, lb: i128) {
	let n = mul.len();
	let mut data = Data { a, mul, cap, lb, score: 0, fixed: vec![!0; n], id: (0..n).collect() };
	reduce(&mut data);
	let mut g = solve_relax(&data);
	data.score += -g.val::<i128>();
	rec(&mut data, &mut g, "");
}

fn main() {
	let (g1, g2) = construct_graph();
	let mut es = vec![];
	let mut mul = vec![];
	let scale = u64::max_value() as i128;
	for i in 0..N3 {
		mul.push(3);
		let a = merge(&merge(&g1[i * 3], &g1[i * 3 + 1]), &g1[i * 3 + 2]).into_iter().map(|(j, w)| (j, (w * 2) as i128 * scale)).collect();
		let b = merge(&merge(&g2[i * 3], &g2[i * 3 + 1]), &g2[i * 3 + 2]).into_iter().map(|(j, w)| (j, (w * 2) as i128)).collect();
		es.push(merge(&a, &b));
	}
	for i in 0..N2 {
		mul.push(2);
		let a = merge(&g1[N3 * 3 + i * 2], &g1[N3 * 3 + i * 2 + 1]).into_iter().map(|(j, w)| (j, (w * 3) as i128 * scale)).collect();
		let b = merge(&g2[N3 * 3 + i * 2], &g2[N3 * 3 + i * 2 + 1]).into_iter().map(|(j, w)| (j, (w * 3) as i128)).collect();
		es.push(merge(&a, &b));
	}
	for i in N3 * 3 + N2 * 2..N {
		mul.push(1);
		let a = g1[i].iter().map(|&(j, w)| (j, (w * 6) as i128 * scale)).collect();
		let b = g2[i].iter().map(|&(j, w)| (j, (w * 6) as i128)).collect();
		es.push(merge(&a, &b));
	}
	solve(es, mul, vec![1000; 1000], 1179959622 * scale + 7703387);
}