#[macro_use]
pub mod common;
pub mod mincostflow;
pub mod mincostcirculation;

use std::io::BufRead;
use std::io::Write;

pub const N1: usize = 1000000;
pub const N2: usize = 1000;
pub const K: usize = 4000;

pub fn read_csv(file: &str) -> Vec<Vec<usize>> {
	let mut list = vec![];
	let reader = std::io::BufReader::new(std::fs::File::open(file).unwrap());
	for line in reader.lines() {
		let mut tmp = vec![];
		for s in line.unwrap().split(',') {
			tmp.push(s.parse().unwrap());
		}
		tmp.remove(0);
		list.push(tmp);
	}
	list
}

pub fn write_solution(ps: &Vec<usize>, file: &str) {
	let mut writer = std::io::BufWriter::new(std::fs::File::create(file).unwrap());
	writeln!(writer, "ChildId,GiftId").unwrap();
	for i in 0..N1 {
		writeln!(writer, "{},{}", i, ps[i]).unwrap();
	}
}

pub fn read_solution(file: &str) -> Vec<usize> {
	let mut list = vec![];
	let mut reader = std::io::BufReader::new(std::fs::File::open(file).unwrap());
	reader.read_line(&mut String::new()).unwrap();
	for line in reader.lines() {
		let s = line.unwrap();
		list.push(s.split(',').nth(1).unwrap().parse().unwrap());
	}
	list
}

pub fn construct_graph() -> Vec<Vec<(usize, i64)>> {
	let cs = read_csv("child_wishlist.csv");
	let gs = read_csv("gift_goodkids.csv");
	let mut g = vec![vec![]; N1];
	for i in 0..N1 {
		for j in 0..10 {
			g[i].push((cs[i][j], (2 * (10 - j as i64) + 1) * 100 * 2));
		}
	}
	for i in 0..N2 {
		for j in 0..1000 {
			g[gs[i][j]].push((i, (2 * (1000 - j as i64) + 1) * 2));
		}
	}
	for i in 0..N1 {
		g[i].sort();
		let mut tmp: Vec<(usize, i64)> = vec![];
		for &(i, w) in &g[i] {
			let k = tmp.len();
			if k > 0 && tmp[k - 1].0 == i {
				tmp[k - 1].1 -= w;
			} else {
				tmp.push((i, -w));
			}
		}
		g[i] = tmp;
	}
	g
}

pub fn merge(a: &Vec<(usize, i64)>, b: &Vec<(usize, i64)>) -> Vec<(usize, i64)> {
	let mut c = vec![];
	let mut i = 0;
	let mut j = 0;
	while i < a.len() && j < b.len() {
		if a[i].0 == b[j].0 {
			c.push((a[i].0, a[i].1 + b[j].1));
			i += 1;
			j += 1;
		} else if a[i].0 < b[j].0 {
			c.push(a[i]);
			i += 1;
		} else {
			c.push(b[j]);
			j += 1;
		}
	}
	c.extend(a[i..].iter());
	c.extend(b[j..].iter());
	c
}

pub fn get_cost(a: &Vec<(usize, i64)>, i: usize) -> Option<i64> {
	for &(j, w) in a {
		if j == i {
			return Some(w);
		}
	}
	None
}

pub fn cost_to_score(cost: i64) -> f64 {
	(-cost - 200000000 - 2000000) as f64 / 4000000000.0
}
