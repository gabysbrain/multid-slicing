extern crate nalgebra as na;

use std::fmt;
use std::slice;
use std::ptr;
use na::{Dim,DMatrix,DVector,MatrixN,VectorN};
use na::allocator::Allocator;
use na::DefaultAllocator;

#[repr(C)]
pub struct SliceSeg {
  p1_1: f64,
  p1_2: f64,
  p2_1: f64,
  p2_2: f64
}

impl fmt::Display for SliceSeg {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "p1.1: {}, p1.2: {}, p2.1: {}, p2.2: {}", 
           self.p1_1, self.p1_2, self.p2_1, self.p2_2)
  }
}

#[no_mangle]
pub extern fn r_spi(s: *const f64, r: usize, c: usize, 
                    fp: *const f64, n: usize, 
                    d1: usize, d2: usize) -> *const SliceSeg {
  
  let s_vec = unsafe { slice::from_raw_parts(s, r*c) };
  let fp_vec = unsafe {  slice::from_raw_parts(fp, n) };
  let ms = DMatrix::from_vec(r, c, s_vec.to_vec());
  let vfp = DVector::from_vec(fp_vec.to_vec());
  //println!("{} {}", ms, vfp);
  
  match spi(&ms, &vfp, d1, d2) {
    Some(x) => &x as *const SliceSeg, 
    None    => ptr::null(),
  }
}

fn spi<N: Dim>(simplex: &MatrixN<f64,N>, 
               focus_pt: &VectorN<f64,N>, 
               d1: usize, d2: usize)
      -> Option<SliceSeg>
    where DefaultAllocator: Allocator<f64, N> + Allocator<f64, N, N>
{
          //S1: StorageMut<N, R, C>,
          //S2: StorageMut<N, R, U1> {
  let n = focus_pt.shape().0 + 1;

  let mut t = DMatrix::repeat(n, n, 1.0);
  // TODO: figure out how to compose this more elegantly
  let simp_t = simplex.transpose();
  for i in 0..(n-1) {
    for j in 0..(n-1) {
      t[(i,j)] = simp_t[(i,j)];
    }
  }
  for i in 0..(n-1) {
    t[(i,n-1)] = focus_pt[i];
  }

  // Check if the fp lies in the matrix
  let mut decomp = na::linalg::LU::new(t.clone());
  //let det_t = decomp.determinant();
  if decomp.determinant() == 0.0 {
    // TODO: add a small (1e-5) amount to the focus point part to move the 
    // point away from the simplex
    for i in 0..(n-1) {
      t[(i,n-1)] += 1e-5;
    }
    decomp = na::linalg::LU::new(t);
  }

  // Compute lambda as best we can
  let mut rr = DVector::repeat(n, 1.0);
  for i in 0..(n-1) {
    rr[i] = focus_pt[i];
  }
  rr[d1] = 0.0;
  rr[d2] = 0.0;
  let mut rr_x = DVector::repeat(n, 0.0);
  let mut rr_y = DVector::repeat(n, 0.0);
  rr_x[d1] = 1.0;
  rr_y[d2] = 1.0;

  // we are trying to compute T^(-1) . [x-xn,y-yn,...,z-zn]
  let mut lambdas = DMatrix::from_columns(&[rr_x, rr_y, rr]);
  //println!("{}", lambdas);
  decomp.solve_mut(&mut lambdas);
  
  // If t is still singular then the simplex lies in the plane
  //if decomp.determinant() == 0.0 {
    //// FIXME: return the simplex itself as the answer
    //println!("missing logic!");
  //}

  
  // find the d1 and d2 ranges that make each lambda 0
  // most indices are based on solving ax + by + c = 0
  // but keeping the other lambdas between 0 and 1

  // put y=mx+b into each other lambda formula and try and get a good range
  // only consider the last lambda
  //println!("{} {} {}", DVector::from_column_slice(&lambdas.column(0).as_slice()), 
                   //DVector::from_column_slice(&lambdas.column(1).as_slice()), 
                   //DVector::from_column_slice(&lambdas.column(2).as_slice()));
  let ranges = ccr(&DVector::from_column_slice(&lambdas.column(0).as_slice()), 
                   &DVector::from_column_slice(&lambdas.column(1).as_slice()), 
                   &DVector::from_column_slice(&lambdas.column(2).as_slice()), 
                   n-1); 
  ranges
}

fn ccr(lx: &VectorN<f64,na::Dynamic>, 
       ly: &VectorN<f64,na::Dynamic>, 
       lc: &VectorN<f64,na::Dynamic>, 
       n: usize) -> Option<SliceSeg> {
  if lx[n] == 0.0 && ly[n] == 0.0 {
    None
  } else if lx[n] == 0.0 {
    ccr_x0(lx, ly, lc, n)
  } else if ly[n] == 0.0 {
    ccr_y0(lx, ly, lc, n)
  } else {
    ccr_general(lx, ly, lc, n)
  }
}

fn ccr_general(lx: &VectorN<f64,na::Dynamic>, 
               ly: &VectorN<f64,na::Dynamic>, 
               lc: &VectorN<f64,na::Dynamic>, 
               n: usize) -> Option<SliceSeg> {
  let mut min_x = 0.0;
  let mut max_x = 0.0;
  let mut min_set = false;
  let mut max_set = false;

  for i in 0..lx.shape().0 {
    if i != n {
      let x = lx[i] - ly[i] * lx[n] / ly[n];
      let c = lc[i] - ly[i] * lc[n] / ly[n];
      let ans = -c / x;
      if x >= 0.0 && (!max_set || ans > max_x) { // see what can be overridden
        max_x = ans;
        max_set = true;
      } else if x < 0.0 && (!min_set || ans < min_x) { // dividing by 0 switches the comparison
        min_x = ans;
        min_set = true;
      }
    }
  }
  // Some final intersection checking
  if !min_set || !max_set || min_x - max_x < -1e-8 {
    None
  } else {
    Some(SliceSeg {
      p1_2: max_x,  // the original R implementation ordered things this way
      p1_1: min_x, 
      p2_2: (-lc[n] - lx[n] * max_x) / ly[n],
      p2_1: (-lc[n] - lx[n] * min_x) / ly[n]
    })
  }
}

// common cross range when lx[n] == 0
fn ccr_x0(lx: &VectorN<f64,na::Dynamic>, 
          ly: &VectorN<f64,na::Dynamic>, 
          lc: &VectorN<f64,na::Dynamic>, 
          n: usize) -> Option<SliceSeg> {
  let mut min_x = 0.0;
  let mut max_x = 0.0;
  let mut min_set = false;
  let mut max_set = false;

  // solve ly * y + c = 0
  let y = -lc[n] / ly[n];
  for i in 0..lx.shape().0 {
    if i != n {
      // replace y in all other formulas and solve lx * x + ly * y + lc >=0 for x
      let c = ly[i] * y + lc[i];
      let ans = -c / lx[i];
      if lx[i] == 0.0 && c < 0.0 {
        return None // No intersection here
      }

      if lx[i] >= 0.0 && (!max_set || ans > max_x) { // see what can be overridden
        max_x = ans;
        max_set = true;
      } else if lx[i] < 0.0 && (!min_set || ans < min_x) { // dividing by 0 switches the comparison
        min_x = ans;
        min_set = true;
      }
    }
  }
  // Some final intersection checking
  if !min_set || !max_set || min_x - max_x < -1e-8 {
    None
  } else {
    Some(SliceSeg {
      p2_1: max_x, // orig R ordered things this way
      p1_1: min_x, 
      p1_2: y,
      p2_2: y
    })
  }
}

// common cross range when ly[n] == 0
fn ccr_y0(lx: &VectorN<f64,na::Dynamic>, 
          ly: &VectorN<f64,na::Dynamic>, 
          lc: &VectorN<f64,na::Dynamic>, 
          n: usize) -> Option<SliceSeg> {
  let mut min_y = 0.0;
  let mut max_y = 0.0;
  let mut min_set = false;
  let mut max_set = false;

  // solve lx * x + c = 0
  let x = -lc[n] / lx[n];
  for i in 0..lx.shape().0 {
    if i != n {
      // replace y in all other formulas and solve lx * x + ly * y + lc >=0 for x
      let c = lx[i] * x + lc[i];
      let ans = -c / ly[i];
      if ly[i] == 0.0 && c < 0.0 {
        return None // No intersection here
      }

      if ly[i] >= 0.0 && (!max_set || ans > max_y) { // see what can be overridden
        max_y = ans;
        max_set = true;
      } else if ly[i] < 0.0 && (!min_set || ans < min_y) { // dividing by 0 switches the comparison
        min_y = ans;
        min_set = true;
      }
    }
  }
  // Some final intersection checking
  if !min_set || !max_set || min_y - max_y < -1e-8 {
    None
  } else {
    Some(SliceSeg {
      p1_1: x,
      p2_1: x,
      p2_2: max_y, // orig R impl orders things backwards
      p1_2: min_y,
    })
  }
}

