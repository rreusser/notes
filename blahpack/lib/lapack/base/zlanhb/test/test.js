/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync; // eslint-disable-line node/no-sync
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var zlanhb = require( './../lib/base.js' );

// FIXTURES //

var upper_max = require( './fixtures/upper_max.json' );
var upper_one = require( './fixtures/upper_one.json' );
var upper_inf = require( './fixtures/upper_inf.json' );
var upper_frob = require( './fixtures/upper_frob.json' );
var lower_max = require( './fixtures/lower_max.json' );
var lower_one = require( './fixtures/lower_one.json' );
var lower_inf = require( './fixtures/lower_inf.json' );
var lower_frob = require( './fixtures/lower_frob.json' );
var edge_1x1_max = require( './fixtures/edge_1x1_max.json' );
var edge_1x1_one = require( './fixtures/edge_1x1_one.json' );
var edge_1x1_inf = require( './fixtures/edge_1x1_inf.json' );
var edge_1x1_frob = require( './fixtures/edge_1x1_frob.json' );
var diag_k0_upper_max = require( './fixtures/diag_k0_upper_max.json' );
var diag_k0_upper_one = require( './fixtures/diag_k0_upper_one.json' );
var diag_k0_upper_inf = require( './fixtures/diag_k0_upper_inf.json' );
var diag_k0_upper_frob = require( './fixtures/diag_k0_upper_frob.json' );
var upper_k1_max = require( './fixtures/upper_k1_max.json' );
var upper_k1_one = require( './fixtures/upper_k1_one.json' );
var upper_k1_inf = require( './fixtures/upper_k1_inf.json' );
var upper_k1_frob = require( './fixtures/upper_k1_frob.json' );
var lower_k1_max = require( './fixtures/lower_k1_max.json' );
var lower_k1_one = require( './fixtures/lower_k1_one.json' );
var lower_k1_inf = require( './fixtures/lower_k1_inf.json' );
var lower_k1_frob = require( './fixtures/lower_k1_frob.json' );

// VARIABLES //

// Band storage for upper Hermitian 5x5 matrix with K=2 (LDAB=3), column-major layout // eslint-disable-line max-len

// Full Hermitian matrix A:

//   [ 1        (-4+2i)  (7-1i)   0        0       ]

//   [ (-4-2i)   5       (-8+3i)  (6-2i)   0       ]

//   [ (7+1i)   (-8-3i)   9       (-3+1i)  (2+4i)  ]

//   [ 0        (6+2i)   (-3-1i)   4       (-1+3i) ]

//   [ 0         0       (2-4i)   (-1-3i)   3      ]

// Upper band storage (LDAB=3, K=2):

//   Row 0 (superdiag-2): AB(1,j) = A(j-2, j)

//   Row 1 (superdiag-1): AB(2,j) = A(j-1, j)

//   Row 2 (diagonal):    AB(3,j) = A(j, j)
var upperAB = new Complex128Array([
	0,
	0,
	0,
	0,
	1,
	0,        // col 0: *, *, (1,0)
	0,
	0,
	-4,
	2,
	5,
	0,        // col 1: *, (-4+2i), (5,0)
	7,
	-1,
	-8,
	3,
	9,
	0,        // col 2: (7-1i), (-8+3i), (9,0)
	6,
	-2,
	-3,
	1,
	4,
	0,        // col 3: (6-2i), (-3+1i), (4,0)
	2,
	4,
	-1,
	3,
	3,
	0         // col 4: (2+4i), (-1+3i), (3,0)
]);

// Band storage for lower Hermitian 5x5 matrix with K=2 (LDAB=3), column-major layout // eslint-disable-line max-len

// Full Hermitian matrix A:

//   [ 2        (-3-1i)  (1+2i)   0        0       ]

//   [ (-3+1i)   6       (-5+3i)  (7-1i)   0       ]

//   [ (1-2i)   (-5-3i)   8       (-2+4i)  (-4+1i) ]

//   [ 0        (7+1i)   (-2-4i)   3       (1+2i)  ]

//   [ 0         0       (-4-1i)  (1-2i)    5      ]

// Lower band storage:

//   Row 0 (diagonal):    AB(1,j) = A(j, j)

//   Row 1 (subdiag-1):   AB(2,j) = A(j+1, j)

//   Row 2 (subdiag-2):   AB(3,j) = A(j+2, j)
var lowerAB = new Complex128Array([
	2,
	0,
	-3,
	1,
	1,
	-2,       // col 0: (2,0), (-3+1i), (1-2i)
	6,
	0,
	-5,
	-3,
	7,
	1,        // col 1: (6,0), (-5-3i), (7+1i)
	8,
	0,
	-2,
	-4,
	-4,
	-1,      // col 2: (8,0), (-2-4i), (-4-1i)
	3,
	0,
	1,
	-2,
	0,
	0,        // col 3: (3,0), (1-2i), *
	5,
	0,
	0,
	0,
	0,
	0         // col 4: (5,0), *, *
]);

// K=0 diagonal matrix (LDAB=1), N=4: diag(3, -7, 2, -4) (real diagonal)
var diagK0 = new Complex128Array([ 3, 0, -7, 0, 2, 0, -4, 0 ]);

// K=1 upper Hermitian 4x4, column-major layout (LDAB=2)

// Full Hermitian matrix A:

//   [ 2        (-3+1i)   0        0       ]

//   [ (-3-1i)   4        (1+2i)   0       ]

//   [ 0        (1-2i)   -5        (6-3i)  ]

//   [ 0         0       (6+3i)    7       ]

// Upper band storage (LDAB=2, K=1):

//   Row 0 (superdiag): AB(1,j) = A(j-1,j)

//   Row 1 (diagonal):  AB(2,j) = A(j,j)
var upperK1 = new Complex128Array([
	0,
	0,
	2,
	0,        // col 0: *, (2,0)
	-3,
	1,
	4,
	0,        // col 1: (-3+1i), (4,0)
	1,
	2,
	-5,
	0,       // col 2: (1+2i), (-5,0)
	6,
	-3,
	7,
	0         // col 3: (6-3i), (7,0)
]);

// K=1 lower Hermitian 4x4, column-major layout (LDAB=2)

// Same Hermitian matrix as upperK1, stored in lower format

// Lower band storage (LDAB=2, K=1):

//   Row 0 (diagonal):  AB(1,j) = A(j,j)

//   Row 1 (subdiag):   AB(2,j) = A(j+1,j)
var lowerK1 = new Complex128Array([
	2,
	0,
	-3,
	-1,      // col 0: (2,0), (-3-1i)
	4,
	0,
	1,
	-2,       // col 1: (4,0), (1-2i)
	-5,
	0,
	6,
	3,        // col 2: (-5,0), (6+3i)
	7,
	0,
	0,
	0         // col 3: (7,0), *
]);

// 1x1 matrix, K=0
var oneByOne = new Complex128Array([ 5, 0 ]);

var work = new Float64Array( 10 );

// FUNCTIONS //

/**
* Asserts that two numbers are approximately equal.
*
* @private
* @param {*} actual - actual value
* @param {*} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 ); // eslint-disable-line max-len
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

/**
* Zeros the work array.
*
* @private
*/
function zeroWork() {
	var i;
	for ( i = 0; i < work.length; i++ ) {
		work[ i ] = 0.0;
	}
}

// TESTS //

test( 'zlanhb: upper_max', function t() {
	var result = zlanhb( 'max', 'upper', 5, 2, upperAB, 1, 3, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = upper_max;
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhb: upper_one', function t() {
	var result;
	var tc;

	zeroWork();
	result = zlanhb( 'one-norm', 'upper', 5, 2, upperAB, 1, 3, 0, work, 1, 0 );
	tc = upper_one;
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhb: upper_inf', function t() {
	var result;
	var tc;

	zeroWork();
	result = zlanhb( 'inf-norm', 'upper', 5, 2, upperAB, 1, 3, 0, work, 1, 0 );
	tc = upper_inf;
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhb: upper_frob', function t() {
	var result = zlanhb( 'frobenius', 'upper', 5, 2, upperAB, 1, 3, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = upper_frob;
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhb: lower_max', function t() {
	var result = zlanhb( 'max', 'lower', 5, 2, lowerAB, 1, 3, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = lower_max;
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhb: lower_one', function t() {
	var result;
	var tc;

	zeroWork();
	result = zlanhb( 'one-norm', 'lower', 5, 2, lowerAB, 1, 3, 0, work, 1, 0 );
	tc = lower_one;
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhb: lower_inf', function t() {
	var result;
	var tc;

	zeroWork();
	result = zlanhb( 'inf-norm', 'lower', 5, 2, lowerAB, 1, 3, 0, work, 1, 0 );
	tc = lower_inf;
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhb: lower_frob', function t() {
	var result = zlanhb( 'frobenius', 'lower', 5, 2, lowerAB, 1, 3, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = lower_frob;
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhb: edge_n0', function t() {
	var result = zlanhb( 'max', 'upper', 0, 2, upperAB, 1, 3, 0, work, 1, 0 ); // eslint-disable-line max-len
	assert.equal( result, 0.0 );
});

test( 'zlanhb: edge_1x1_max', function t() {
	var result = zlanhb( 'max', 'upper', 1, 0, oneByOne, 1, 1, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = edge_1x1_max;
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhb: edge_1x1_one', function t() {
	var result;
	var tc;

	zeroWork();
	result = zlanhb( 'one-norm', 'upper', 1, 0, oneByOne, 1, 1, 0, work, 1, 0 );
	tc = edge_1x1_one;
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhb: edge_1x1_inf', function t() {
	var result;
	var tc;

	zeroWork();
	result = zlanhb( 'inf-norm', 'upper', 1, 0, oneByOne, 1, 1, 0, work, 1, 0 );
	tc = edge_1x1_inf;
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhb: edge_1x1_frob', function t() {
	var result = zlanhb( 'frobenius', 'upper', 1, 0, oneByOne, 1, 1, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = edge_1x1_frob;
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhb: returns 0 for unknown norm type', function t() {
	var result = zlanhb( 'unknown', 'upper', 4, 1, upperK1, 1, 2, 0, work, 1, 0 ); // eslint-disable-line max-len
	assert.equal( result, 0.0 );
});

test( 'zlanhb: diag_k0_upper_max', function t() {
	var result = zlanhb( 'max', 'upper', 4, 0, diagK0, 1, 1, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = diag_k0_upper_max;
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhb: diag_k0_upper_one', function t() {
	var result;
	var tc;

	zeroWork();
	result = zlanhb( 'one-norm', 'upper', 4, 0, diagK0, 1, 1, 0, work, 1, 0 );
	tc = diag_k0_upper_one;
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhb: diag_k0_upper_inf', function t() {
	var result;
	var tc;

	zeroWork();
	result = zlanhb( 'inf-norm', 'upper', 4, 0, diagK0, 1, 1, 0, work, 1, 0 );
	tc = diag_k0_upper_inf;
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhb: diag_k0_upper_frob', function t() {
	var result = zlanhb( 'frobenius', 'upper', 4, 0, diagK0, 1, 1, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = diag_k0_upper_frob;
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhb: upper_k1_max', function t() {
	var result = zlanhb( 'max', 'upper', 4, 1, upperK1, 1, 2, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = upper_k1_max;
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhb: upper_k1_one', function t() {
	var result;
	var tc;

	zeroWork();
	result = zlanhb( 'one-norm', 'upper', 4, 1, upperK1, 1, 2, 0, work, 1, 0 );
	tc = upper_k1_one;
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhb: upper_k1_inf', function t() {
	var result;
	var tc;

	zeroWork();
	result = zlanhb( 'inf-norm', 'upper', 4, 1, upperK1, 1, 2, 0, work, 1, 0 );
	tc = upper_k1_inf;
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhb: upper_k1_frob', function t() {
	var result = zlanhb( 'frobenius', 'upper', 4, 1, upperK1, 1, 2, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = upper_k1_frob;
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhb: lower_k1_max', function t() {
	var result = zlanhb( 'max', 'lower', 4, 1, lowerK1, 1, 2, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = lower_k1_max;
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhb: lower_k1_one', function t() {
	var result;
	var tc;

	zeroWork();
	result = zlanhb( 'one-norm', 'lower', 4, 1, lowerK1, 1, 2, 0, work, 1, 0 );
	tc = lower_k1_one;
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhb: lower_k1_inf', function t() {
	var result;
	var tc;

	zeroWork();
	result = zlanhb( 'inf-norm', 'lower', 4, 1, lowerK1, 1, 2, 0, work, 1, 0 );
	tc = lower_k1_inf;
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhb: lower_k1_frob', function t() {
	var result = zlanhb( 'frobenius', 'lower', 4, 1, lowerK1, 1, 2, 0, work, 1, 0 ); // eslint-disable-line max-len
	var tc = lower_k1_frob;
	assertClose( result, tc.result, 1e-14, 'result' );
});
