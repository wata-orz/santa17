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

struct Entry<W>(V, W);
impl_cmp!(Entry<W>; |a, b| b.1.partial_cmp(&a.1).unwrap(); where W: PartialOrd);

impl<C, W> Graph<C, W> where	C: Copy + Default + Ord + Sub<Output = C> + Neg<Output = C> + AddAssign + SubAssign + From<u8>,
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
		let mut eps = W::default();
		for v in 0..n {
			self.p[v] *= (n as u32 + 1).into();
		}
		for v in 0..n {
			for e in &mut self.es[v] {
				e.cost *= (n as u32 + 1).into();
				if e.cap > C::default() {
					eps.setmax(-(e.cost + self.p[v] - self.p[e.to]));
				}
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
	pub fn fitting(&mut self) {
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
	pub fn check_potential(&self) {
		let n = self.es.len();
		for u in 0..n {
			for e in &self.es[u] {
				if e.cap > C::default() {
					assert!(e.cost + self.p[u] - self.p[e.to] >= W::default());
				}
			}
		}
	}
	pub fn inc(&mut self, v: V, to: V) -> W {
		let e = self.es[v].iter().position(|e| e.to == to).unwrap();
		let r = self.es[v][e].rev;
		let d = self.es[v][e].cost + self.p[v];
		let (fixed, dp) = self.shortest(to, v, d);
		if fixed[v] && self.es[v][e].cost + self.p[v] - self.p[to] < W::default() {
			self.augment(to, v, dp);
			self.es[to][r].cap += 1.into();
			self.es[v][e].cost + self.p[v] - self.p[to]
		} else {
			self.es[v][e].cap += 1.into();
			W::default()
		}
		// self.check_potential();
	}
	pub fn dec(&mut self, v: V, to: V) -> W {
		let e = self.es[v].iter().position(|e| e.to == to).unwrap();
		let r = self.es[v][e].rev;
		if self.es[v][e].cap == C::default() {
			let (fixed, dp) = self.shortest(v, to, W::default());
			assert!(fixed[to]);
			let w = self.es[v][e].cost + self.p[v] - self.p[to];
			self.augment(v, to, dp);
			self.es[to][r].cap -= 1.into();
			-w
		} else {
			self.es[v][e].cap -= 1.into();
			W::default()
		}
		// self.check_potential();
	}
	fn augment(&mut self, s: V, t: V, dp: Vec<(W, usize)>) {
		let mut v = t;
		while v != s {
			let i = dp[v].1;
			let e = self.es[v][i];
			self.es[e.to][e.rev].cap -= 1.into();
			self.es[v][i].cap += 1.into();
			v = e.to;
		}
	}
	fn shortest(&mut self, s: V, t: V, d: W) -> (Vec<bool>, Vec<(W, usize)>) {
		let n = self.es.len();
		let mut fixed = vec![false; n];
		let mut dp = vec![(W::default(), !0); n];
		let mut que = ::std::collections::BinaryHeap::new();
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
		if fixed[t] {
			for v in 0..self.es.len() { if fixed[v] { self.p[v] += dp[v].0 - dp[t].0 } }
		} else {
			for v in 0..self.es.len() { if fixed[v] { self.p[v] += d } }
		}
		(fixed, dp)
	}
}

