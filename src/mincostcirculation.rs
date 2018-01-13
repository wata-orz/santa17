/// Andrew V. Goldberg, Robert E. Tarjan: Finding Minimum-Cost Circulations by Successive Approximation.
/// Math. Oper. Res. 15(3): 430-466 (1990)

use common::*;
use std::ops::*;

type V = usize;

#[derive(Copy, Clone, Debug)]
pub struct E<C, W> {
	pub to: V,
	pub cap: C,
	pub init: C,
	pub cost: W,
	pub rev: usize
}

#[derive(Clone, Debug)]
pub struct Graph<C, W> {
	pub es: Vec<Vec<E<C, W>>>,
	pub ex: Vec<C>,
	pub p: Vec<W>,
	iter: Vec<usize>
}

impl<C, W> Graph<C, W> where	C: Copy + Default + Ord + Sub<Output = C> + Neg<Output = C> + AddAssign + SubAssign,
								W: Copy + Default + Ord + Add<Output = W> + Sub<Output = W> + Mul<Output = W> + Div<Output = W> + Neg<Output = W>
														+ AddAssign + SubAssign + MulAssign + DivAssign + From<u32> + ::std::fmt::Debug {
	pub fn new(n: usize) -> Graph<C, W> {
		Graph { es: vec![vec![]; n], ex: vec![C::default(); n], p: vec![W::default(); n], iter: vec![0; n] }
	}
	pub fn add(&mut self, v: V, to: V, cap: C, cost: W) {
		let (fwd, rev) = (self.es[v].len(), self.es[to].len());
		self.es[v].push(E { to: to, cap: cap, init: cap, cost: cost, rev: rev });
		self.es[to].push(E { to: v, cap: C::default(), init: C::default(), cost: -cost, rev: fwd });
	}
	fn is_admissible(&self, v: V, e: &E<C, W>) -> bool {
		e.cap > C::default() && e.cost + self.p[v] - self.p[e.to] < W::default()
	}
	/// Compute minimum cost circulation.
	/// Return whether there is a flow satisfying the demand constraints.
	/// flow(e) = init(e) - cap(e).
	/// For solving min cost s-t flow of value F, set ex(s)=F and ex(t)=-F.
	/// For every vertex, the total capacity of its incident edges must be fit in C.
	/// C and W are expected to be i64 or Float (do not use f64).
	/// Dual: minimize \sum_v ex(v)p(v) + \sum_{uv} cap(e) max(0, -cost(uv) - p(u) + p(v)).
	/// To get the dual optimum, call fitting if W is integer, or use p/(n+1) if W is float.
	/// O(V^2 E log VC), where C=max(cost(e)). When cap=1, O(V E log VC).
	pub fn solve(&mut self) -> bool {
		let n = self.es.len();
		let mut eps = W::from(2);
		for v in &mut self.es {
			for e in v {
				e.cost *= (n as u32 + 1).into();
				eps.setmax(e.cost);
			}
		}
		let mut stack = vec![];
		let mut visit = vec![false; n];
		let mut ok = false;
		'refine: while { eps /= 2.into(); eps > W::default() } {
			eprintln!("{:?}", eps);
			for v in 0..n {
				for i in 0..self.es[v].len() {
					let e = self.es[v][i];
					if self.is_admissible(v, &e) {
						self.ex[e.to] += e.cap;
						self.ex[v] -= e.cap;
						self.es[e.to][e.rev].cap += e.cap;
						self.es[v][i].cap = C::default();
					}
				}
			}
			loop {
				for v in 0..n {
					self.iter[v] = 0;
					if self.ex[v] > C::default() {
						visit[v] = true;
						stack.push(v);
					} else {
						visit[v] = false;
					}
				}
				if stack.len() == 0 { break }
				while let Some(v) = stack.pop() {
					for e in &self.es[v] {
						if !visit[e.to] && self.is_admissible(v, e) {
							visit[e.to] = true;
							stack.push(e.to);
						}
					}
				}
				if (0..n).filter(|&v| visit[v]).flat_map(|v| self.es[v].iter()).all(|e| e.cap <= C::default() || visit[e.to]) {
					assert!(!ok);
					break 'refine;
				}
				for v in (0..n).filter(|&v| visit[v]) { self.p[v] -= eps }
				for v in 0..n {
					while self.ex[v] > C::default() {
						let f = ok!(self.dfs(v, self.ex[v]));
						if f == C::default() { break }
						else { self.ex[v] -= f }
					}
				}
			}
			ok = true;
		}
		for v in &mut self.es {
			for e in v {
				e.cost /= (n as u32 + 1).into();
			}
		}
		ok
	}
	fn dfs(&mut self, v: V, f: C) -> C {
		if self.ex[v] < C::default() {
			let d = ::std::cmp::min(f, -self.ex[v]);
			self.ex[v] += d;
			return d;
		}
		while self.iter[v] < self.es[v].len() {
			let e = self.es[v][self.iter[v]];
			if self.is_admissible(v, &e) {
				let d = self.dfs(e.to, ::std::cmp::min(f, e.cap));
				if d > C::default() {
					self.es[v][self.iter[v]].cap -= d;
					self.es[e.to][e.rev].cap += d;
					return d;
				}
			}
			self.iter[v] += 1;
		}
		C::default()
	}
	pub fn val<T>(&self) -> T where T: Default + From<C> + From<W> + AddAssign + Mul<Output = T> {
		let mut tot = T::default();
		for v in &self.es {
			for e in v {
				if e.cap < e.init {
					tot += T::from(e.init - e.cap) * e.cost.into();
				}
			}
		}
		tot
	}
	/// Find p s.t. cost(uv) + p(u) - p(v) >= 0 holds for every edge uv with cap(uv) > 0.
	/// W is expected to be i64. When W is float, use p/(n+1).
	/// O(E log V)
	pub fn fitting(&mut self) where W: ::std::fmt::Debug {
		let n = self.es.len();
		let mut d: Vec<W> = self.p.iter().map(|&a| a / (n as u32 + 1).into()).collect(); // p must be non-positive.
		let mut d2: Vec<W> = (0..n).map(|v| d[v] * (n as u32 + 1).into() - self.p[v] + 1.into()).collect();
		let mut fixed = vec![false; n];
		let mut que = ::std::collections::BinaryHeap::new();
		for v in 0..n {
			que.push((-d2[v], v))
		}
		while let Some((_, v)) = que.pop() {
			if fixed[v] { continue }
			fixed[v] = true;
			for e in &self.es[v] {
				if e.cap > C::default() && !fixed[e.to] && ok!(d2[e.to].setmin(d2[v] + e.cost * (n as u32 + 1).into() + self.p[v] - self.p[e.to] + 1.into())) {
					d[e.to] = d[v] + e.cost;
					que.push((-d2[e.to], e.to));
				}
			}
		}
		self.p = d
	}
}


#[test]
fn test_matching() {
	use rand::{self, Rng};
	let mut rng: rand::XorShiftRng = rand::random();
	for _ in 0..100 {
		let n = rng.gen_range(1, 10);
		let m = rng.gen_range(1, 10);
		let mut cost = mat![!0; n; m];
		let p = rng.next_f64();
		for i in 0..n {
			for j in 0..m {
				if rng.next_f64() < p {
					cost[i][j] = rng.gen_range(0, 10);
				}
			}
		}
		let mut g: Graph<i64, i64> = Graph::new(n + m + 2);
		for i in 0..n {
			g.add(n + m, i, 1, 0);
		}
		for j in 0..m {
			g.add(n + j, n + m + 1, 1, 0);
		}
		const INF: i32 = 1000000000;
		for i in 0..n {
			for j in 0..m {
				if cost[i][j] != !0 {
					g.add(i, n + j, INF as i64, cost[i][j] as i64);
				}
			}
		}
		g.add(n + m + 1, n + m, INF as i64, -INF as i64);
		g.solve();
		let mut f = 0;
		for e in &g.es[n + m + 1] { if e.to == n + m { f = e.init - e.cap } }
		let tot = g.val::<i64>() + INF as i64 * f;
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
				if f2.setmax(j.count_ones()) {
					tot2 = dp[n][j];
				} else if f2 == j.count_ones() {
					tot2.setmin(dp[n][j]);
				}
			}
		}
		assert_eq!(f, f2 as i64);
		assert_eq!(tot, tot2 as i64);
		g.fitting();
		for v in 0..g.es.len() {
			for e in &g.es[v] {
				if e.cap > 0 {
					assert!(e.cost + g.p[v] - g.p[e.to] >= 0);
				}
			}
		}
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

#[test]
fn test_flow() {
	use rand::{self, Rng};
	let mut rnd: rand::XorShiftRng = rand::random();
	for _ in 0..100 {
		let n = 2 + 10 * 5;
		let (s, t) = (50, 51);
		const INF: usize = 1 << 29;
		let mut g = ::graph::dinic::Graph::new(n);
		let mut g2: Graph<i64, i64> = Graph::new(n);
		for i in 0..10 {
			g.add(s, i, INF);
			g.add(40 + i, t, INF);
			g2.add(s, i, INF as i64, 0);
			g2.add(40 + i, t, INF as i64, 0);
			for j in 0..4 {
				for k in 0..10 {
					let d = rnd.gen_range(0, 1000);
					g.add(10 * j + i, 10 * j + 10 + k, d);
					g2.add(10 * j + i, 10 * j + 10 + k, d as i64, 0);
				}
			}
		}
		let (flow, _) = g.clone().solve(s, t);
		g2.ex[s] = flow as i64 + 1;
		g2.ex[t] = -(flow as i64) - 1;
		assert!(!g2.solve());
	}
}

#[test]
fn test_int() {
	use rand::{self, Rng};
	let mut rng: rand::XorShiftRng = rand::random();
	for _ in 0..100 {
		let n = 20;
		let d = 10000;
		let mut g: Graph<i64, i64> = Graph::new(n);
		for i in 0..n {
			for j in 0..n {
				if i != j {
					g.add(i, j, rng.gen_range(1, d), rng.gen_range(-d, d));
				}
			}
		}
		assert!(g.solve());
		let cost = g.val::<i64>();
		g.fitting();
		let mut dual = 0;
		for v in 0..n {
			for e in &g.es[v] {
				if e.cap > 0 {
					assert!(e.cost + g.p[v] - g.p[e.to] >= 0);
				}
				dual += e.init * ::std::cmp::max(0, -e.cost - g.p[v] + g.p[e.to]);
			}
		}
		assert_eq!(cost, -dual);
	}
}

#[test]
fn test_float() {
	use rand::{self, Rng};
	let mut rng: rand::XorShiftRng = rand::random();
	define_eps!(F; 1e-10);
	for _ in 0..100 {
		let n = 10;
		let mut g: Graph<F, F> = Graph::new(n);
		for i in 0..n {
			for j in 0..n {
				if i != j {
					g.add(i, j, F::new(rng.next_f64()), F::new(rng.next_f64() - 0.5));
				}
			}
		}
		assert!(g.solve());
		let cost = g.val::<F>();
		for i in 0..n {
			g.p[i] /= F::new(n as f64 + 1.0);
		}
		let mut dual = F::new(0.0);
		for v in 0..n {
			for e in &g.es[v] {
				if e.cap > F::new(0.0) {
					assert!(e.cost + g.p[v] - g.p[e.to] >= F::new(0.0));
				}
				dual += e.init * ::std::cmp::max(F::new(0.0), -e.cost - g.p[v] + g.p[e.to]);
			}
		}
		assert_eq!(cost, -dual);
	}
}
