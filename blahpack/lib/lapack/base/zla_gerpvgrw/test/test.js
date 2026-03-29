'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var zla_gerpvgrw = require( './../lib' );


// FUNCTIONS //

/**
* Creates a column-major complex matrix from interleaved re/im values in row-major order (for readability).
*
* @private
* @param {number} M - number of rows
* @param {number} N - number of columns
* @param {Array} vals - interleaved `[re,im,...]` values in row-major order
* @returns {Complex128Array} column-major complex array
*/
function colMajor( M, N, vals ) {
	var out = new Float64Array( 2 * M * N );
	var i;
	var j;
	for ( i = 0; i < M; i += 1 ) {
		for ( j = 0; j < N; j += 1 ) {
			out[ 2 * ( j * M + i ) ] = vals[ 2 * ( i * N + j ) ];
			out[ ( 2 * ( j * M + i ) ) + 1 ] = vals[ ( 2 * ( i * N + j ) ) + 1 ];
		}
	}
	return new Complex128Array( out.buffer );
}


// TESTS //

test( 'zla_gerpvgrw: main export is a function', function t() {
	assert.strictEqual( typeof zla_gerpvgrw, 'function' );
});

test( 'zla_gerpvgrw: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof zla_gerpvgrw.ndarray, 'function' );
});

test( 'zla_gerpvgrw: basic 3x3 complex matrix', function t() {
	var result;
	var A;
	var AF;

	// A = [ (6,1)  (2,0)  (1,1) ]
	//     [ (2,3)  (5,2)  (3,0) ]
	//     [ (1,0)  (3,1)  (4,2) ]
	A = colMajor( 3, 3, [
		6, 1, 2, 0, 1, 1,
		2, 3, 5, 2, 3, 0,
		1, 0, 3, 1, 4, 2
	]);

	// AF upper triangular (hand-specified as if from LU)
	AF = colMajor( 3, 3, [
		6, 1, 2, 0, 1, 1,
		0, 0, 4, 2, 2.5, 0,
		0, 0, 0, 0, 2, 1
	]);

	result = zla_gerpvgrw.ndarray( 3, 3, A, 1, 3, 0, AF, 1, 3, 0 );

	// Col 0: CABS1 of A col: 7,5,1 => amax=7; AF col: 7 => umax=7; ratio=1.0
	// Col 1: CABS1 of A col: 2,7,4 => amax=7; AF col: 2,6 => umax=6; ratio=7/6
	// Col 2: CABS1 of A col: 2,3,6 => amax=6; AF col: 2,2.5,3 => umax=3; ratio=2.0
	// rpvgrw = min(1.0, 7/6, 2.0) = 1.0
	assert.strictEqual( result, 1.0 );
});

test( 'zla_gerpvgrw: ncols=0 returns 1.0', function t() {
	var result;
	var A;
	var AF;

	A = new Complex128Array( [ 1, 2, 3, 4 ] );
	AF = new Complex128Array( [ 1, 2, 3, 4 ] );
	result = zla_gerpvgrw.ndarray( 3, 0, A, 1, 3, 0, AF, 1, 3, 0 );
	assert.strictEqual( result, 1.0 );
});

test( 'zla_gerpvgrw: 1x1 complex matrix with equal A and AF', function t() {
	var result;
	var A;
	var AF;

	A = new Complex128Array( [ 5, 3 ] );
	AF = new Complex128Array( [ 5, 3 ] );

	// CABS1 = 8 for both => ratio = 1.0
	result = zla_gerpvgrw.ndarray( 1, 1, A, 1, 1, 0, AF, 1, 1, 0 );
	assert.strictEqual( result, 1.0 );
});

test( 'zla_gerpvgrw: 1x1 with pivot growth (ratio=0.5)', function t() {
	var result;
	var A;
	var AF;

	A = new Complex128Array( [ 3, 1 ] );
	AF = new Complex128Array( [ 6, 2 ] );

	// CABS1(A) = 4, CABS1(AF) = 8 => ratio = 0.5
	result = zla_gerpvgrw.ndarray( 1, 1, A, 1, 1, 0, AF, 1, 1, 0 );
	assert.strictEqual( result, 0.5 );
});

test( 'zla_gerpvgrw: zero U column (umax=0) is skipped', function t() {
	var result;
	var A;
	var AF;

	A = colMajor( 3, 3, [
		4, 1, 0, 0, 3, 0,
		2, 0, 0, 0, 1, 1,
		1, 2, 0, 0, 2, 0
	]);
	AF = colMajor( 3, 3, [
		4, 1, 0, 0, 3, 0,
		0, 0, 0, 0, 1, 1,
		0, 0, 0, 0, 2, 0
	]);

	result = zla_gerpvgrw.ndarray( 3, 3, A, 1, 3, 0, AF, 1, 3, 0 );
	assert.strictEqual( result, 1.0 );
});

test( 'zla_gerpvgrw: ratio less than one', function t() {
	var result;
	var A;
	var AF;

	A = colMajor( 2, 2, [
		1, 1, 2, 0,
		3, 0, 1, 3
	]);
	AF = colMajor( 2, 2, [
		5, 3, 4, 4,
		0, 0, 6, 2
	]);

	result = zla_gerpvgrw.ndarray( 2, 2, A, 1, 2, 0, AF, 1, 2, 0 );

	// Col 0: amax=max(2,3)=3, umax=max(8)=8, ratio=0.375
	// Col 1: amax=max(2,4)=4, umax=max(8,8)=8, ratio=0.5
	// rpvgrw = 0.375
	assert.strictEqual( result, 0.375 );
});

test( 'zla_gerpvgrw: ncols < N processes only first ncols columns', function t() {
	var result;
	var A;
	var AF;

	A = colMajor( 3, 3, [
		4, 0, 2, 1, 99, 0,
		1, 0, 5, 0, 99, 0,
		3, 0, 3, 3, 99, 0
	]);
	AF = colMajor( 3, 3, [
		4, 0, 2, 1, 99, 0,
		0, 0, 5, 0, 99, 0,
		0, 0, 0, 0, 99, 0
	]);

	result = zla_gerpvgrw.ndarray( 3, 2, A, 1, 3, 0, AF, 1, 3, 0 );

	// Only columns 0-1:
	// Col 0: amax=max(4,1,3)=4, umax=max(4)=4, ratio=1.0
	// Col 1: amax=max(3,5,6)=6, umax=max(3,5)=5, ratio=1.2
	// rpvgrw = 1.0
	assert.strictEqual( result, 1.0 );
});

test( 'zla_gerpvgrw: purely imaginary elements', function t() {
	var result;
	var A;
	var AF;

	A = colMajor( 2, 2, [
		0, 4, 0, 2,
		0, 6, 0, 5
	]);
	AF = colMajor( 2, 2, [
		0, 8, 0, 3,
		0, 0, 0, 10
	]);

	result = zla_gerpvgrw.ndarray( 2, 2, A, 1, 2, 0, AF, 1, 2, 0 );

	// Col 0: amax=max(4,6)=6, umax=max(8)=8, ratio=0.75
	// Col 1: amax=max(2,5)=5, umax=max(3,10)=10, ratio=0.5
	// rpvgrw = 0.5
	assert.strictEqual( result, 0.5 );
});

test( 'zla_gerpvgrw: large values', function t() {
	var result;
	var A;
	var AF;

	A = colMajor( 2, 2, [
		1e100, 0, 2e100, 0,
		3e100, 0, 4e100, 0
	]);
	AF = colMajor( 2, 2, [
		6e100, 0, 5e100, 0,
		0, 0, 8e100, 0
	]);

	result = zla_gerpvgrw.ndarray( 2, 2, A, 1, 2, 0, AF, 1, 2, 0 );

	// Col 0: amax=3e100, umax=6e100, ratio=0.5
	// Col 1: amax=4e100, umax=max(5e100,8e100)=8e100, ratio=0.5
	// rpvgrw = 0.5
	assert.strictEqual( result, 0.5 );
});

test( 'zla_gerpvgrw: works with non-unit strides and offsets', function t() {
	var result;
	var A;
	var AF;

	// 2x2 matrix stored with stride1=2 (padding between rows) and stride2=4
	A = new Complex128Array( [
		1, 1, 999, 999, 3, 0, 999, 999,
		2, 0, 999, 999, 1, 3, 999, 999
	]);
	AF = new Complex128Array( [
		5, 3, 999, 999, 0, 0, 999, 999,
		4, 4, 999, 999, 6, 2, 999, 999
	]);

	result = zla_gerpvgrw.ndarray( 2, 2, A, 2, 4, 0, AF, 2, 4, 0 );

	// Col 0: amax=max(2,3)=3, umax=max(8)=8, ratio=0.375
	// Col 1: amax=max(2,4)=4, umax=max(8,8)=8, ratio=0.5
	// rpvgrw = 0.375
	assert.strictEqual( result, 0.375 );
});

test( 'zla_gerpvgrw: works with offset', function t() {
	var result;
	var A;
	var AF;

	// 1x1 matrix starting at offset 2 (in complex elements)
	A = new Complex128Array( [ 0, 0, 0, 0, 3, 1, 0, 0 ] );
	AF = new Complex128Array( [ 0, 0, 0, 0, 6, 2, 0, 0 ] );

	// CABS1(A) = 4, CABS1(AF) = 8 => ratio = 0.5
	result = zla_gerpvgrw.ndarray( 1, 1, A, 1, 1, 2, AF, 1, 1, 2 );
	assert.strictEqual( result, 0.5 );
});

test( 'zla_gerpvgrw: BLAS-style entry via main export', function t() {
	var result;
	var A;
	var AF;

	A = colMajor( 2, 2, [
		1, 1, 2, 0,
		3, 0, 1, 3
	]);
	AF = colMajor( 2, 2, [
		5, 3, 4, 4,
		0, 0, 6, 2
	]);

	// BLAS-style signature: ( N, ncols, A, LDA, AF, LDAF )
	result = zla_gerpvgrw( 2, 2, A, 2, AF, 2 );
	assert.strictEqual( result, 0.375 );
});

test( 'zla_gerpvgrw: negative real and imaginary parts', function t() {
	var result;
	var A;
	var AF;

	A = colMajor( 2, 2, [
		-3, -1, -7, 0,
		-5, 0, -2, -4
	]);
	AF = colMajor( 2, 2, [
		10, 0, 7, 0,
		0, 0, 4, 0
	]);

	result = zla_gerpvgrw.ndarray( 2, 2, A, 1, 2, 0, AF, 1, 2, 0 );

	// CABS1 uses absolute values, so signs don't matter:
	// Col 0: amax=max(4,5)=5, umax=max(10)=10, ratio=0.5
	// Col 1: amax=max(7,6)=7, umax=max(7,4)=7, ratio=1.0
	// rpvgrw = 0.5
	assert.strictEqual( result, 0.5 );
});
