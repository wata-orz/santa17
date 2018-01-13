use std::ops::*;

type V = usize;

#[derive(Copy, Clone, Debug)]
pub struct E<C, W> {
	pub to: V,
	pub cap: C,
	pub cost: W,
	pub rev: usize
}

#[derive(Clone, Debug)]
pub struct Graph<C, W> {
	pub es: Vec<Vec<E<C, W>>>,
	pub p: Vec<W>
}

struct Entry<W>(V, W);
impl_cmp!(Entry<W>; |a, b| b.1.partial_cmp(&a.1).unwrap(); where W: PartialOrd);

impl<C, W> Graph<C, W> where	C: Copy + Default + PartialOrd + AddAssign + SubAssign + From<u8>,
								W: Copy + Default + PartialOrd + Add<Output = W> + Sub<Output = W> + AddAssign + Neg<Output = W> {
	pub fn new(n: usize) -> Graph<C, W> {
		Graph { es: vec![vec![]; n], p: vec![W::default(); n] }
	}
	/// cap must be an integer.
	pub fn add(&mut self, v: V, to: V, cap: C, cost: W) {
		let (fwd, rev) = (self.es[v].len(), self.es[to].len());
		self.es[v].push(E { to: to, cap: cap, cost: cost, rev: rev });
		self.es[to].push(E { to: v, cap: C::default(), cost: -cost, rev: fwd });
	}
	/// Compute minimum cost s-t flow.
	/// Return (flow value F, total cost).
	/// To constraint the flow to the maximum, set `saturate` true.
	/// flow(e) = before.cap(e) - after.cap(e).
	/// For every edge uv with cap(uv) > 0, cost(uv) + p(u) - p(v) >= 0 holds.
	/// Dual: minimize \sum_v ex(v)p(v) + \sum_{uv} cap(e) max(0, -cost(uv) - p(u) + p(v)).
	/// O(V E + F E log V).
	pub fn solve(&mut self, s: V, t: V, saturate: bool) -> (C, W) {
		let n = self.es.len();
		loop {
			let mut update = false;
			for u in 0..n {
				for e in &self.es[u] {
					if e.cap > C::default() && self.p[e.to] > self.p[u] + e.cost {
						self.p[e.to] = self.p[u] + e.cost;
						update = true;
					}
				}
			}
			if !update { break }
		}
		let mut f = C::default();
		let mut tot = W::default();
		let mut fixed = vec![false; n];
		let mut dp = vec![(W::default(), !0); n];
		let mut que = ::std::collections::BinaryHeap::new();
		loop {
			for a in &mut fixed { *a = false }
			for a in &mut dp { *a = (W::default(), !0) }
			que.clear();
			que.push(Entry(s, W::default()));
			while let Some(Entry(u, d)) = que.pop() {
				if fixed[u] { continue }
				fixed[u] = true;
				if u == t { break }
				for e in &self.es[u] {
					let v = e.to;
					let d2 = d + e.cost + self.p[u] - self.p[v];
					if e.cap > C::default() && !fixed[v] && (dp[v].1 == !0 || dp[v].0 > d2) {
						dp[v] = (d2, e.rev);
						que.push(Entry(v, d2));
					}
				}
			}
			if !fixed[t] || !saturate && dp[t].0 + self.p[t] - self.p[s] >= W::default() { return (f, tot) }
			for v in 0..n { if fixed[v] { self.p[v] += dp[v].0 - dp[t].0 } }
			f += 1.into();
			tot += self.p[t] - self.p[s];
			let mut v = t;
			while v != s {
				let i = dp[v].1;
				let e = self.es[v][i];
				self.es[e.to][e.rev].cap -= 1.into();
				self.es[v][i].cap += 1.into();
				v = e.to;
			}
		}
	}
}


#[test]
fn test() {
	use rand::{self, Rng};
	let mut rng: rand::XorShiftRng = rand::random();
	for _ in 0..100 {
		let saturate: bool = rng.gen();
		let n = rng.gen_range(1, 10);
		let m = rng.gen_range(1, 10);
		let mut cost = mat![!0; n; m];
		let p = rng.next_f64();
		for i in 0..n {
			for j in 0..m {
				if rng.next_f64() < p {
					cost[i][j] = rng.gen_range(0, 10) * if saturate { 1 } else { -1 };
				}
			}
		}
		let mut g = Graph::new(n + m + 2);
		for i in 0..n {
			g.add(n + m, i, 1, 0);
		}
		for j in 0..m {
			g.add(n + j, n + m + 1, 1, 0);
		}
		for i in 0..n {
			for j in 0..m {
				if cost[i][j] != !0 {
					g.add(i, n + j, i32::max_value(), cost[i][j]);
				}
			}
		}
		let (f, tot) = g.solve(n + m, n + m + 1, saturate);
		const INF: i32 = 1000000000;
		let mut dp = mat![INF; n + 1; 1 << m];
		dp[0][0] = 0;
		for i in 0..n {
			dp[i + 1] = dp[i].clone();
			for j in 0..1<<m {
				if dp[i][j] != INF {
					for k in 0..m {
						if cost[i][k] != !0 && j >> k & 1 == 0 {
							ok!(dp[i + 1][j | 1 << k].setmin(dp[i][j] + cost[i][k]));
						}
					}
				}
			}
		}
		use ::common::*;
		let mut f2 = 0;
		let mut tot2 = 0;
		for j in 0..1<<m {
			if dp[n][j] != INF {
				if saturate {
					if f2.setmax(j.count_ones()) {
						tot2 = dp[n][j];
					} else if f2 == j.count_ones() {
						tot2.setmin(dp[n][j]);
					}
				} else {
					tot2.setmin(dp[n][j]);
				}
			}
		}
		if saturate { assert_eq!(f, f2 as i32); }
		assert_eq!(tot, tot2);
		let mut dual = f * (g.p[n + m] - g.p[n + m + 1]);
		for i in 0..n {
			let w = g.p[n + m] - g.p[i];
			if w < 0 { dual -= w }
		}
		for j in 0..m {
			let w = g.p[n + j] - g.p[n + m + 1];
			if w < 0 { dual -= w }
		}
		assert_eq!(tot, -dual);
	}
}
