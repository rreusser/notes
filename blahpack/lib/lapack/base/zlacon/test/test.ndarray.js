/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Complex128 = require( '@stdlib/complex/float64/ctor' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var Int32Array = require( '@stdlib/array/int32' );
var zgemv = require( '../../../../blas/base/zgemv/lib/base.js' );
var zlacon = require( './../lib' );


// FUNCTIONS //

/**
* Run the zlacon reverse communication loop for a given complex matrix A.
*
* @private
* @param {number} N - matrix order
* @param {Float64Array} Av - matrix A stored column-major as interleaved re/im Float64
* @returns {Object} - { est, iterations }
*/
function runEstimate( N, Av ) {
	var KASE = new Int32Array( 1 );
	var iter = 0;
	var zero = new Complex128( 0.0, 0.0 );
	var EST = new Float64Array( 1 );
	var tmp = new Complex128Array( N );
	var one = new Complex128( 1.0, 0.0 );
	var V = new Complex128Array( N );
	var X = new Complex128Array( N );

	KASE[ 0 ] = 0;
	EST[ 0 ] = 0.0;

	while ( true ) {
		zlacon.ndarray( N, V, 1, 0, X, 1, 0, EST, KASE );
		if ( KASE[ 0 ] === 0 ) {
			break;
		}
		iter += 1;
		if ( iter > 20 ) {
			break;
		}
		if ( KASE[ 0 ] === 1 ) {
			// tmp = A * X
			zgemv( 'no-transpose', N, N, one, new Complex128Array( Av.buffer ), 1, N, 0, X, 1, 0, zero, tmp, 1, 0 ); // eslint-disable-line max-len
			copyCV( tmp, X, N );
		} else {
			// tmp = A^H * X
			zgemv( 'conjugate-transpose', N, N, one, new Complex128Array( Av.buffer ), 1, N, 0, X, 1, 0, zero, tmp, 1, 0 ); // eslint-disable-line max-len
			copyCV( tmp, X, N );
		}
	}
	return {
		'est': EST[ 0 ],
		'iterations': iter
	};
}

/**
* Copy complex array src to dst.
*
* @private
* @param {Complex128Array} src - source array
* @param {Complex128Array} dst - destination array
* @param {number} N - number of elements
*/
function copyCV( src, dst, N ) {
	var sv = reinterpret( src, 0 );
	var dv = reinterpret( dst, 0 );
	var i;
	for ( i = 0; i < (N * 2); i++ ) {
		dv[ i ] = sv[ i ];
	}
}

/**
* Build a column-major Float64Array for a complex matrix from a 2D array of [re, im] pairs.
* entries[i][j] = [re, im] for row i, col j.
*
* @private
* @param {number} N - matrix order
* @param {Array} entries - 2D array of [re, im] pairs
* @returns {Float64Array} column-major interleaved matrix
*/
function buildMatrix( N, entries ) {
	var A = new Float64Array( (N * N) * 2 );
	var i;
	var j;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < N; i++ ) {
			A[ ((j * N) + i) * 2 ] = entries[ i ][ j ][ 0 ];
			A[ (((j * N) + i) * 2) + 1 ] = entries[ i ][ j ][ 1 ];
		}
	}
	return A;
}


// TESTS //

test( 'zlacon: main export is a function', function t() {
	assert.strictEqual( typeof zlacon, 'function' );
});

test( 'zlacon: attached to the main export is an `ndarray` method', function t() { // eslint-disable-line max-len
	assert.strictEqual( typeof zlacon.ndarray, 'function' );
});

test( 'zlacon: 3x3 identity matrix (1-norm = 1)', function t() {
	var entries;
	var result;
	var A;

	entries = [
		[ [1, 0], [0, 0], [0, 0] ],
		[ [0, 0], [1, 0], [0, 0] ],
		[ [0, 0], [0, 0], [1, 0] ]
	];
	A = buildMatrix( 3, entries );
	result = runEstimate( 3, A );
	assert.ok( Math.abs( result.est - 1.0 ) < 1e-10, 'est should be 1.0, got ' + result.est ); // eslint-disable-line max-len
	assert.strictEqual( result.iterations, 4 );
});

test( 'zlacon: 1x1 complex matrix (3+4i), 1-norm = 5', function t() {
	var result;
	var A;

	A = new Float64Array( [ 3.0, 4.0 ] );
	result = runEstimate( 1, A );
	assert.ok( Math.abs( result.est - 5.0 ) < 1e-10, 'est should be 5.0, got ' + result.est ); // eslint-disable-line max-len
	assert.strictEqual( result.iterations, 1 );
});

test( 'zlacon: 4x4 complex diagonal matrix, 1-norm = 5', function t() {
	var entries;
	var result;
	var A;

	entries = [
		[ [1, 2], [0, 0], [0, 0], [0, 0] ],
		[ [0, 0], [3, 4], [0, 0], [0, 0] ],
		[ [0, 0], [0, 0], [0, 1], [0, 0] ],
		[ [0, 0], [0, 0], [0, 0], [2, 0] ]
	];
	A = buildMatrix( 4, entries );
	result = runEstimate( 4, A );
	assert.ok( Math.abs( result.est - 5.0 ) < 1e-10, 'est should be 5.0, got ' + result.est ); // eslint-disable-line max-len
	assert.strictEqual( result.iterations, 5 );
});

test( 'zlacon: 3x3 dense complex matrix', function t() {
	var entries;
	var result;
	var A;

	entries = [
		[ [1, 1], [2, -1], [0, 0] ],
		[ [0, 0], [3, 2], [-1, 1] ],
		[ [4, -3], [0, 0], [2, 1] ]
	];
	A = buildMatrix( 3, entries );
	result = runEstimate( 3, A );
	assert.ok( Math.abs( result.est - 5.84161925296377937 ) < 1e-10, 'est should be ~5.8416, got ' + result.est ); // eslint-disable-line max-len
	assert.strictEqual( result.iterations, 5 );
});

test( 'zlacon: 5x5 upper triangular complex matrix', function t() {
	var entries;
	var result;
	var A;

	entries = [
		[ [2, 1], [1, 0], [0, 1], [1, 1], [0, 0] ],
		[ [0, 0], [3, -1], [2, 0], [0, 0], [1, 2] ],
		[ [0, 0], [0, 0], [1, 1], [1, -1], [0, 0] ],
		[ [0, 0], [0, 0], [0, 0], [4, 0], [2, 1] ],
		[ [0, 0], [0, 0], [0, 0], [0, 0], [1, -2] ]
	];
	A = buildMatrix( 5, entries );
	result = runEstimate( 5, A );
	assert.ok( Math.abs( result.est - 6.82842712474618985 ) < 1e-10, 'est should be ~6.828, got ' + result.est ); // eslint-disable-line max-len
	assert.strictEqual( result.iterations, 5 );
});

test( 'zlacon: KASE=0 initializes X to 1/N and sets KASE=1', function t() {
	var KASE;
	var EST;
	var xv;
	var N;
	var V;
	var X;
	var i;

	N = 3;
	V = new Complex128Array( N );
	X = new Complex128Array( N );
	EST = new Float64Array( 1 );
	KASE = new Int32Array( 1 );
	KASE[ 0 ] = 0;
	zlacon.ndarray( N, V, 1, 0, X, 1, 0, EST, KASE );
	assert.strictEqual( KASE[ 0 ], 1, 'KASE should be 1 after init' );
	xv = reinterpret( X, 0 );
	for ( i = 0; i < N; i++ ) {
		assert.ok( Math.abs( xv[ (i * 2) ] - (1.0 / N) ) < 1e-15, 'X[' + i + '] real should be 1/N' ); // eslint-disable-line max-len
		assert.ok( Math.abs( xv[ (i * 2) + 1 ] ) < 1e-15, 'X[' + i + '] imag should be 0' ); // eslint-disable-line max-len
	}
});

test( 'zlacon: N=1 returns immediately after first A*X multiplication', function t() { // eslint-disable-line max-len
	var KASE;
	var EST;
	var xv;
	var N;
	var V;
	var X;

	N = 1;
	V = new Complex128Array( N );
	X = new Complex128Array( N );
	EST = new Float64Array( 1 );
	KASE = new Int32Array( 1 );
	KASE[ 0 ] = 0;
	zlacon.ndarray( N, V, 1, 0, X, 1, 0, EST, KASE );
	assert.strictEqual( KASE[ 0 ], 1 );
	xv = reinterpret( X, 0 );
	xv[ 0 ] = 3.0;
	xv[ 1 ] = 4.0;
	zlacon.ndarray( N, V, 1, 0, X, 1, 0, EST, KASE );
	assert.strictEqual( KASE[ 0 ], 0, 'KASE should be 0 for N=1 after first multiply' ); // eslint-disable-line max-len
	assert.ok( Math.abs( EST[ 0 ] - 5.0 ) < 1e-10, 'EST should be 5.0' );
});

test( 'zlacon: estimate is a lower bound for the true 1-norm', function t() {
	var trueNorm;
	var entries;
	var colSums;
	var result;
	var re;
	var im;
	var A;
	var i;
	var j;

	entries = [
		[ [1, 1], [2, -1], [0, 0] ],
		[ [0, 0], [3, 2], [-1, 1] ],
		[ [4, -3], [0, 0], [2, 1] ]
	];
	A = buildMatrix( 3, entries );
	result = runEstimate( 3, A );
	colSums = [ 0, 0, 0 ];
	for ( j = 0; j < 3; j++ ) {
		for ( i = 0; i < 3; i++ ) {
			re = entries[ i ][ j ][ 0 ];
			im = entries[ i ][ j ][ 1 ];
			colSums[ j ] += Math.sqrt( (re * re) + (im * im) );
		}
	}
	trueNorm = Math.max.apply( null, colSums );
	assert.ok( result.est <= trueNorm + 1e-10, 'estimate should not exceed true 1-norm' ); // eslint-disable-line max-len
	assert.ok( result.est > 0, 'estimate should be positive' );
});

test( 'zlacon: works with non-unit stride for V and X', function t() {
	var KASE;
	var EST;
	var xv;
	var N;
	var V;
	var X;

	N = 1;
	V = new Complex128Array( 3 );
	X = new Complex128Array( 3 );
	EST = new Float64Array( 1 );
	KASE = new Int32Array( 1 );
	KASE[ 0 ] = 0;
	zlacon.ndarray( N, V, 2, 0, X, 2, 0, EST, KASE );
	assert.strictEqual( KASE[ 0 ], 1 );
	xv = reinterpret( X, 0 );
	xv[ 0 ] = 7.0;
	xv[ 1 ] = 0.0;
	zlacon.ndarray( N, V, 2, 0, X, 2, 0, EST, KASE );
	assert.strictEqual( KASE[ 0 ], 0 );
	assert.ok( Math.abs( EST[ 0 ] - 7.0 ) < 1e-10, 'est should be 7.0' );
});

test( 'zlacon: works with offset for V and X', function t() {
	var KASE;
	var EST;
	var xv;
	var N;
	var V;
	var X;

	N = 1;
	V = new Complex128Array( 3 );
	X = new Complex128Array( 3 );
	EST = new Float64Array( 1 );
	KASE = new Int32Array( 1 );
	KASE[ 0 ] = 0;
	zlacon.ndarray( N, V, 1, 1, X, 1, 1, EST, KASE );
	assert.strictEqual( KASE[ 0 ], 1 );
	xv = reinterpret( X, 0 );
	assert.ok( Math.abs( xv[ 2 ] - 1.0 ) < 1e-15, 'X at offset 1 real should be 1.0' ); // eslint-disable-line max-len
	assert.ok( Math.abs( xv[ 3 ] ) < 1e-15, 'X at offset 1 imag should be 0' );
	xv[ 2 ] = 2.0;
	xv[ 3 ] = 3.0;
	zlacon.ndarray( N, V, 1, 1, X, 1, 1, EST, KASE );
	assert.strictEqual( KASE[ 0 ], 0 );
	assert.ok( Math.abs( EST[ 0 ] - Math.sqrt( 13 ) ) < 1e-10, 'est should be sqrt(13)' ); // eslint-disable-line max-len
});

test( 'zlacon: cycling detection works (EST does not increase)', function t() {
	var entries;
	var result;
	var A;

	entries = [
		[ [1, 0], [0, 0] ],
		[ [0, 0], [1, 0] ]
	];
	A = buildMatrix( 2, entries );
	result = runEstimate( 2, A );
	assert.ok( Math.abs( result.est - 1.0 ) < 1e-10, 'est should be 1.0' );
	assert.ok( result.iterations <= 20, 'should terminate' );
});

test( 'zlacon: purely imaginary matrix', function t() {
	var entries;
	var result;
	var A;

	entries = [
		[ [0, 3], [0, 0] ],
		[ [0, 0], [0, 7] ]
	];
	A = buildMatrix( 2, entries );
	result = runEstimate( 2, A );
	assert.ok( Math.abs( result.est - 7.0 ) < 1e-10, 'est should be 7.0, got ' + result.est ); // eslint-disable-line max-len
});

test( 'zlacon: scaled identity matrix', function t() {
	var entries;
	var result;
	var A;

	entries = [
		[ [100, 0], [0, 0], [0, 0] ],
		[ [0, 0], [100, 0], [0, 0] ],
		[ [0, 0], [0, 0], [100, 0] ]
	];
	A = buildMatrix( 3, entries );
	result = runEstimate( 3, A );
	assert.ok( Math.abs( result.est - 100.0 ) < 1e-8, 'est should be 100.0, got ' + result.est ); // eslint-disable-line max-len
});
