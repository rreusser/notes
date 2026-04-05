'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var zlanhs = require( './../lib/base.js' );

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

// 3x3 upper Hessenberg matrix (interleaved complex, col-major):
// [1+1i  2+0i  3+0i ]
// [4+2i  5+1i  6+0i ]
// [  0   7+3i  8+2i ]
var A = new Complex128Array([
	1,1,  4,2,  0,0,   // col 0
	2,0,  5,1,  7,3,   // col 1
	3,0,  6,0,  8,2    // col 2
]);

test( 'zlanhs: N=0 returns 0', function t() {
	assert.equal( zlanhs( 'max', 0, A, 1, 3, 0, new Float64Array(1), 1, 0 ), 0.0 );
});

test( 'zlanhs: max norm', function t() {
	// Max |a_ij| = max of sqrt(re^2+im^2) over upper Hessenberg entries
	// |8+2i| = sqrt(68) ≈ 8.246
	// |7+3i| = sqrt(58) ≈ 7.616
	// max = sqrt(68)
	var result = zlanhs( 'max', 3, A, 1, 3, 0, new Float64Array(3), 1, 0 );
	assertClose( result, Math.sqrt(68), 1e-14, 'max_norm' );
});

test( 'zlanhs: one-norm (max column sum)', function t() {
	// Col 0: |1+1i| + |4+2i| = sqrt(2) + sqrt(20)
	// Col 1: |2+0i| + |5+1i| + |7+3i| = 2 + sqrt(26) + sqrt(58)
	// Col 2: |3+0i| + |6+0i| + |8+2i| = 3 + 6 + sqrt(68)
	var col2 = 3 + 6 + Math.sqrt(68);
	var result = zlanhs( 'one-norm', 3, A, 1, 3, 0, new Float64Array(3), 1, 0 );
	assertClose( result, col2, 1e-14, 'one_norm' );
});

test( 'zlanhs: infinity-norm (max row sum)', function t() {
	// Row 0: |1+1i| + |2+0i| + |3+0i| = sqrt(2) + 2 + 3
	// Row 1: |4+2i| + |5+1i| + |6+0i| = sqrt(20) + sqrt(26) + 6
	// Row 2: |7+3i| + |8+2i| = sqrt(58) + sqrt(68)
	var row1 = Math.sqrt(20) + Math.sqrt(26) + 6;
	var row2 = Math.sqrt(58) + Math.sqrt(68);
	var expected = Math.max( Math.sqrt(2) + 5, row1, row2 );
	var result = zlanhs( 'inf-norm', 3, A, 1, 3, 0, new Float64Array(3), 1, 0 );
	assertClose( result, expected, 1e-14, 'inf_norm' );
});

test( 'zlanhs: Frobenius norm', function t() {
	// sum of |a_ij|^2 over upper Hessenberg entries
	// = (1+1) + (16+4) + (4+0) + (25+1) + (49+9) + (9+0) + (36+0) + (64+4) = 223
	var result = zlanhs( 'frobenius', 3, A, 1, 3, 0, new Float64Array(3), 1, 0 );
	assertClose( result, Math.sqrt(223), 1e-14, 'frob_norm' );
});

test( 'zlanhs: unrecognized norm returns 0', function t() {
	var result = zlanhs( 'X', 3, A, 1, 3, 0, new Float64Array(3), 1, 0 );
	assert.strictEqual( result, 0.0 );
});

test( 'zlanhs: NaN propagation through max norm', function t() {
	var B = new Complex128Array([
		NaN,0,  1,0,  0,0,
		0,0,    1,0,  1,0,
		0,0,    0,0,  1,0
	]);
	var result = zlanhs( 'max', 3, B, 1, 3, 0, new Float64Array(3), 1, 0 );
	assert.ok( result !== result, 'expected NaN' );
});

test( 'zlanhs: NaN propagation through one norm', function t() {
	var B = new Complex128Array([
		NaN,0,  1,0,  0,0,
		0,0,    1,0,  1,0,
		0,0,    0,0,  1,0
	]);
	var result = zlanhs( 'one-norm', 3, B, 1, 3, 0, new Float64Array(3), 1, 0 );
	assert.ok( result !== result, 'expected NaN' );
});

test( 'zlanhs: NaN propagation through inf norm', function t() {
	var B = new Complex128Array([
		NaN,0,  1,0,  0,0,
		0,0,    1,0,  1,0,
		0,0,    0,0,  1,0
	]);
	var result = zlanhs( 'inf-norm', 3, B, 1, 3, 0, new Float64Array(3), 1, 0 );
	assert.ok( result !== result, 'expected NaN' );
});
