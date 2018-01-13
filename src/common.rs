#[macro_export]
macro_rules! debug {
	($($v:expr),*) => { {
		use ::std::io::Write;
		$(let _ = write!(::std::io::stderr(), "{} = {:?} ", stringify!($v), $v);)*
		let _ = writeln!(::std::io::stderr(), "@ {}:{}", file!(), line!());
	} }
}

#[macro_export]
macro_rules! mat {
	($e:expr) => { $e };
	($e:expr; $d:expr $(; $ds:expr)*) => { vec![mat![$e $(; $ds)*]; $d] };
}

pub trait SetMin {
	fn setmin(&mut self, v: Self) -> bool;
}

impl<T> SetMin for T where T: PartialOrd {
	#[inline]
	fn setmin(&mut self, v: T) -> bool {
		*self > v && { *self = v; true }
	}
}

pub trait SetMax {
	fn setmax(&mut self, v: Self) -> bool;
}

impl<T> SetMax for T where T: PartialOrd {
	#[inline]
	fn setmax(&mut self, v: T) -> bool {
		*self < v && { *self = v; true }
	}
}

#[macro_export]
macro_rules! ok {
	($a:ident$([$i:expr])*.$f:ident()$(@$t:ident)*) => {
		$a$([$i])*.$f($($t),*)
	};
	($a:ident$([$i:expr])*.$f:ident($e:expr$(,$es:expr)*)$(@$t:ident)*) => { {
		let t = $e;
		ok!($a$([$i])*.$f($($es),*)$(@$t)*@t)
	} };
}

macro_rules! impl_cmp {
	($name:ident $(<$($t:ident),*>)*; |$x:ident, $y:ident| $e:expr; $($w:tt)*) => {
		impl $(<$($t),*>)* Ord for $name $(<$($t),*>)* $($w)* {
			#[inline]
			fn cmp(&self, $y: &Self) -> ::std::cmp::Ordering {
				let $x = &self;
				$e
			}
		}
		impl $(<$($t),*>)* PartialOrd for $name $(<$($t),*>)* $($w)* {
			#[inline]
			fn partial_cmp(&self, a: &Self) -> Option<::std::cmp::Ordering> {
				Some(self.cmp(a))
			}
		}
		impl $(<$($t),*>)* PartialEq for $name $(<$($t),*>)* $($w)* {
			#[inline]
			fn eq(&self, a: &Self) -> bool {
				self.cmp(a) == ::std::cmp::Ordering::Equal
			}
		}
		impl $(<$($t),*>)* Eq for $name $(<$($t),*>)* $($w)* {}
	}
}

#[derive(Clone, Copy, Debug, Default, Hash)]
pub struct Tot<T: PartialOrd>(pub T);
impl_cmp!(Tot<T>; |a, b| a.0.partial_cmp(&b.0).unwrap(); where T: PartialOrd);

#[derive(Clone, Copy, Debug, Default, Hash)]
pub struct Rev<T: PartialOrd>(pub T);
impl_cmp!(Rev<T>; |a, b| b.0.partial_cmp(&a.0).unwrap(); where T: PartialOrd);
