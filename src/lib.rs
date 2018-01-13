#[macro_use]
pub mod common;
pub mod mincostcirculation;

use std::io::BufRead;
use std::io::Write;

pub const N: usize = 1000000;
pub const N3: usize = 1667;
pub const N2: usize = 20000;

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
	for i in 0..ps.len() {
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

pub fn construct_graph() -> (Vec<Vec<(usize, i64)>>, Vec<Vec<(usize, i64)>>) {
	let cs = read_csv("child_wishlist_v2.csv");
	let gs = read_csv("gift_goodkids_v2.csv");
	let mut g = vec![vec![]; N];
	for i in 0..N {
		for j in 0..100 {
			g[i].push((cs[i][j], 2 * (100 - j as i64) + 1));
		}
		g[i].sort();
	}
	let mut g2 = vec![vec![]; N];
	for i in 0..1000 {
		for j in 0..1000 {
			g2[gs[i][j]].push((i, 2 * (1000 - j as i64) + 1));
		}
	}
	for i in 0..N {
		g2[i].sort();
	}
	(g, g2)
}

pub fn merge<T: std::ops::Add<Output = T> + Copy>(a: &Vec<(usize, T)>, b: &Vec<(usize, T)>) -> Vec<(usize, T)> {
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

pub fn get_cost<T: Copy>(a: &Vec<(usize, T)>, i: usize) -> Option<T> {
	for &(j, w) in a {
		if j == i {
			return Some(w);
		}
	}
	None
}

pub fn get_score(score1: i64, score2: i64) -> f64 {
	((score1 - 1000000) as f64 / 200000000.0).powf(3.0) + ((score2 - 1000000) as f64 / 2000000000.0).powf(3.0)
}
