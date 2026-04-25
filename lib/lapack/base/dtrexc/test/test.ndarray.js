/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dtrexc = require( './../lib/ndarray.js' );

// FIXTURES //

var swap_1x1_forward = require( './fixtures/swap_1x1_forward.json' );
var swap_1x1_backward = require( './fixtures/swap_1x1_backward.json' );
var swap_1x1_forward_compq_n = require( './fixtures/swap_1x1_forward_compq_n.json' );
var forward_2x2_block = require( './fixtures/forward_2x2_block.json' );
var backward_2x2_block = require( './fixtures/backward_2x2_block.json' );
var forward_1x1_across_2x2 = require( './fixtures/forward_1x1_across_2x2.json' );
var backward_1x1_across_2x2 = require( './fixtures/backward_1x1_across_2x2.json' );
var ifst_adjusted_2x2 = require( './fixtures/ifst_adjusted_2x2.json' );
var backward_1x1_compq_n = require( './fixtures/backward_1x1_compq_n.json' );
var forward_2x2_across_2x2 = require( './fixtures/forward_2x2_across_2x2.json' );
var backward_2x2_across_2x2 = require( './fixtures/backward_2x2_across_2x2.json' );
var ilst_adjusted_fwd = require( './fixtures/ilst_adjusted_fwd.json' );
var backward_2x2_compq_n = require( './fixtures/backward_2x2_compq_n.json' );

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
* Asserts that two arrays are element-wise approximately equal.
*
* @private
* @param {*} actual - actual value
* @param {*} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

/**
* BuildSchurMatrix.
*
* @private
* @param {*} entries - entries
* @param {*} N - N
* @returns {*} result
*/
function buildSchurMatrix( entries, N ) {
	var T = new Float64Array( N * N );
	var i;
	for ( i = 0; i < entries.length; i++ ) {
		T[ entries[ i ][ 0 ] + entries[ i ][ 1 ] * N ] = entries[ i ][ 2 ];
	}
	return T;
}

/**
* Eye.
*
* @private
* @param {*} N - N
* @returns {*} result
*/
function eye( N ) {
	var Q = new Float64Array( N * N );
	var i;
	for ( i = 0; i < N; i++ ) {
		Q[ i + i * N ] = 1.0;
	}
	return Q;
}

/**
* Converts a typed array to a plain array.
*
* @private
* @param {TypedArray} arr - input array
* @returns {Array} output array
*/
function toArray( arr ) {
	var out = [];
	var i;
	for ( i = 0; i < arr.length; i++ ) {
		out.push( arr[ i ] );
	}
	return out;
}

// TESTS //

test( 'dtrexc: swap 1x1 forward', function t() {
	var WORK;
	var tc;
	var N;
	var T;
	var Q;
	var r;

	tc = swap_1x1_forward;
	N = 4;
	T = buildSchurMatrix([
		[0, 0, 4],
		[0, 1, 1],
		[0, 2, 0.5],
		[0, 3, 0.2],
		[1, 1, 3],
		[1, 2, 0.8],
		[1, 3, 0.3],
		[2, 2, 2],
		[2, 3, 0.6],
		[3, 3, 1]
	], N);
	Q = eye( N );
	WORK = new Float64Array( N );
	r = dtrexc( 'update', N, T, 1, N, 0, Q, 1, N, 0, 1, 4, WORK, 1, 0 );
	assert.strictEqual( r.info, tc.info, 'info' );
	assertArrayClose( toArray( T ), tc.T, 1e-12, 'T' );
	assertArrayClose( toArray( Q ), tc.Q, 1e-12, 'Q' );
});

test( 'dtrexc: N=1 quick return', function t() {
	var WORK;
	var T;
	var Q;
	var r;

	T = new Float64Array([ 5.0 ]);
	Q = new Float64Array([ 1.0 ]);
	WORK = new Float64Array( 1 );
	r = dtrexc( 'update', 1, T, 1, 1, 0, Q, 1, 1, 0, 1, 1, WORK, 1, 0 );
	assert.strictEqual( r.info, 0, 'info' );
});

test( 'dtrexc: swap 1x1 backward', function t() {
	var WORK;
	var tc;
	var N;
	var T;
	var Q;
	var r;

	tc = swap_1x1_backward;
	N = 4;
	T = buildSchurMatrix([
		[0, 0, 4],
		[0, 1, 1],
		[0, 2, 0.5],
		[0, 3, 0.2],
		[1, 1, 3],
		[1, 2, 0.8],
		[1, 3, 0.3],
		[2, 2, 2],
		[2, 3, 0.6],
		[3, 3, 1]
	], N);
	Q = eye( N );
	WORK = new Float64Array( N );
	r = dtrexc( 'update', N, T, 1, N, 0, Q, 1, N, 0, 4, 1, WORK, 1, 0 );
	assert.strictEqual( r.info, tc.info, 'info' );
	assertArrayClose( toArray( T ), tc.T, 1e-12, 'T' );
	assertArrayClose( toArray( Q ), tc.Q, 1e-12, 'Q' );
});

test( 'dtrexc: swap 1x1 forward, COMPQ=N', function t() {
	var WORK;
	var tc;
	var N;
	var T;
	var Q;
	var r;

	tc = swap_1x1_forward_compq_n;
	N = 4;
	T = buildSchurMatrix([
		[0, 0, 4],
		[0, 1, 1],
		[0, 2, 0.5],
		[0, 3, 0.2],
		[1, 1, 3],
		[1, 2, 0.8],
		[1, 3, 0.3],
		[2, 2, 2],
		[2, 3, 0.6],
		[3, 3, 1]
	], N);
	Q = eye( N );
	WORK = new Float64Array( N );
	r = dtrexc( 'none', N, T, 1, N, 0, Q, 1, N, 0, 1, 4, WORK, 1, 0 );
	assert.strictEqual( r.info, tc.info, 'info' );
	assertArrayClose( toArray( T ), tc.T, 1e-12, 'T' );
	assertArrayClose( toArray( Q ), tc.Q, 1e-12, 'Q unchanged' );
});

test( 'dtrexc: forward 2x2 block', function t() {
	var WORK;
	var tc;
	var N;
	var T;
	var Q;
	var r;

	tc = forward_2x2_block;
	N = 5;
	T = buildSchurMatrix([
		[0, 0, 3],
		[0, 1, 2],
		[0, 2, 0.5],
		[0, 3, 0.2],
		[0, 4, 0.1],
		[1, 0, -2],
		[1, 1, 3],
		[1, 2, 0.8],
		[1, 3, 0.3],
		[1, 4, 0.15],
		[2, 2, 5],
		[2, 3, 0.6],
		[2, 4, 0.4],
		[3, 3, 1],
		[3, 4, 0.9],
		[4, 4, 0.5]
	], N);
	Q = eye( N );
	WORK = new Float64Array( N );
	r = dtrexc( 'update', N, T, 1, N, 0, Q, 1, N, 0, 1, 4, WORK, 1, 0 );
	assert.strictEqual( r.info, tc.info, 'info' );
	assertArrayClose( toArray( T ), tc.T, 1e-12, 'T' );
	assertArrayClose( toArray( Q ), tc.Q, 1e-12, 'Q' );
});

test( 'dtrexc: backward 2x2 block', function t() {
	var WORK;
	var tc;
	var N;
	var T;
	var Q;
	var r;

	tc = backward_2x2_block;
	N = 5;
	T = buildSchurMatrix([
		[0, 0, 5],
		[0, 1, 0.5],
		[0, 2, 0.2],
		[0, 3, 0.1],
		[0, 4, 0.3],
		[1, 1, 1],
		[1, 2, 0.6],
		[1, 3, 0.4],
		[1, 4, 0.15],
		[2, 2, 0.5],
		[2, 3, 0.9],
		[2, 4, 0.2],
		[3, 3, 3],
		[3, 4, 2],
		[4, 3, -2],
		[4, 4, 3]
	], N);
	Q = eye( N );
	WORK = new Float64Array( N );
	r = dtrexc( 'update', N, T, 1, N, 0, Q, 1, N, 0, 4, 1, WORK, 1, 0 );
	assert.strictEqual( r.info, tc.info, 'info' );
	assertArrayClose( toArray( T ), tc.T, 1e-12, 'T' );
	assertArrayClose( toArray( Q ), tc.Q, 1e-12, 'Q' );
});

test( 'dtrexc: forward 1x1 across 2x2', function t() {
	var WORK;
	var tc;
	var N;
	var T;
	var Q;
	var r;

	tc = forward_1x1_across_2x2;
	N = 5;
	T = buildSchurMatrix([
		[0, 0, 5],
		[0, 1, 0.5],
		[0, 2, 0.2],
		[0, 3, 0.1],
		[0, 4, 0.3],
		[1, 1, 3],
		[1, 2, 2],
		[1, 3, 0.6],
		[1, 4, 0.4],
		[2, 1, -2],
		[2, 2, 3],
		[2, 3, 0.8],
		[2, 4, 0.15],
		[3, 3, 1],
		[3, 4, 0.9],
		[4, 4, 0.5]
	], N);
	Q = eye( N );
	WORK = new Float64Array( N );
	r = dtrexc( 'update', N, T, 1, N, 0, Q, 1, N, 0, 1, 5, WORK, 1, 0 );
	assert.strictEqual( r.info, tc.info, 'info' );
	assertArrayClose( toArray( T ), tc.T, 1e-12, 'T' );
	assertArrayClose( toArray( Q ), tc.Q, 1e-12, 'Q' );
});

test( 'dtrexc: backward 1x1 across 2x2', function t() {
	var WORK;
	var tc;
	var N;
	var T;
	var Q;
	var r;

	tc = backward_1x1_across_2x2;
	N = 5;
	T = buildSchurMatrix([
		[0, 0, 1],
		[0, 1, 0.5],
		[0, 2, 0.2],
		[0, 3, 0.1],
		[0, 4, 0.3],
		[1, 1, 3],
		[1, 2, 2],
		[1, 3, 0.6],
		[1, 4, 0.4],
		[2, 1, -2],
		[2, 2, 3],
		[2, 3, 0.8],
		[2, 4, 0.15],
		[3, 3, 5],
		[3, 4, 0.9],
		[4, 4, 0.5]
	], N);
	Q = eye( N );
	WORK = new Float64Array( N );
	r = dtrexc( 'update', N, T, 1, N, 0, Q, 1, N, 0, 5, 1, WORK, 1, 0 );
	assert.strictEqual( r.info, tc.info, 'info' );
	assertArrayClose( toArray( T ), tc.T, 1e-12, 'T' );
	assertArrayClose( toArray( Q ), tc.Q, 1e-12, 'Q' );
});

test( 'dtrexc: IFST==ILST (no-op)', function t() {
	var WORK;
	var N;
	var T;
	var Q;
	var r;

	N = 3;
	T = buildSchurMatrix([
		[0, 0, 4],
		[0, 1, 1],
		[0, 2, 0.5],
		[1, 1, 3],
		[1, 2, 0.8],
		[2, 2, 2]
	], N);
	Q = eye( N );
	WORK = new Float64Array( N );
	r = dtrexc( 'update', N, T, 1, N, 0, Q, 1, N, 0, 2, 2, WORK, 1, 0 );
	assert.strictEqual( r.info, 0, 'info' );
});

test( 'dtrexc: IFST points to second row of 2x2 block', function t() {
	var WORK;
	var tc;
	var N;
	var T;
	var Q;
	var r;

	tc = ifst_adjusted_2x2;
	N = 4;
	T = buildSchurMatrix([
		[0, 0, 3],
		[0, 1, 2],
		[0, 2, 0.5],
		[0, 3, 0.2],
		[1, 0, -2],
		[1, 1, 3],
		[1, 2, 0.8],
		[1, 3, 0.3],
		[2, 2, 5],
		[2, 3, 0.6],
		[3, 3, 1]
	], N);
	Q = eye( N );
	WORK = new Float64Array( N );
	r = dtrexc( 'update', N, T, 1, N, 0, Q, 1, N, 0, 2, 4, WORK, 1, 0 );
	assert.strictEqual( r.info, tc.info, 'info' );
	assertArrayClose( toArray( T ), tc.T, 1e-12, 'T' );
	assertArrayClose( toArray( Q ), tc.Q, 1e-12, 'Q' );
});

test( 'dtrexc: backward 1x1, COMPQ=N', function t() {
	var WORK;
	var tc;
	var N;
	var T;
	var Q;
	var r;

	tc = backward_1x1_compq_n;
	N = 4;
	T = buildSchurMatrix([
		[0, 0, 4],
		[0, 1, 1],
		[0, 2, 0.5],
		[0, 3, 0.2],
		[1, 1, 3],
		[1, 2, 0.8],
		[1, 3, 0.3],
		[2, 2, 2],
		[2, 3, 0.6],
		[3, 3, 1]
	], N);
	Q = eye( N );
	WORK = new Float64Array( N );
	r = dtrexc( 'none', N, T, 1, N, 0, Q, 1, N, 0, 3, 1, WORK, 1, 0 );
	assert.strictEqual( r.info, tc.info, 'info' );
	assertArrayClose( toArray( T ), tc.T, 1e-12, 'T' );
});

test( 'dtrexc: forward 2x2 across 2x2', function t() {
	var WORK;
	var tc;
	var N;
	var T;
	var Q;
	var r;

	tc = forward_2x2_across_2x2;
	N = 6;
	T = buildSchurMatrix([
		[0, 0, 4],
		[0, 1, 1],
		[0, 2, 0.5],
		[0, 3, 0.3],
		[0, 4, 0.2],
		[0, 5, 0.1],
		[1, 0, -1],
		[1, 1, 4],
		[1, 2, 0.8],
		[1, 3, 0.4],
		[1, 4, 0.25],
		[1, 5, 0.15],
		[2, 2, 2],
		[2, 3, 3],
		[2, 4, 0.6],
		[2, 5, 0.35],
		[3, 2, -3],
		[3, 3, 2],
		[3, 4, 0.7],
		[3, 5, 0.45],
		[4, 4, 1],
		[4, 5, 0.9],
		[5, 5, 0.5]
	], N);
	Q = eye( N );
	WORK = new Float64Array( N );
	r = dtrexc( 'update', N, T, 1, N, 0, Q, 1, N, 0, 1, 4, WORK, 1, 0 );
	assert.strictEqual( r.info, tc.info, 'info' );
	assertArrayClose( toArray( T ), tc.T, 1e-12, 'T' );
	assertArrayClose( toArray( Q ), tc.Q, 1e-12, 'Q' );
});

test( 'dtrexc: backward 2x2 across 2x2', function t() {
	var WORK;
	var tc;
	var N;
	var T;
	var Q;
	var r;

	tc = backward_2x2_across_2x2;
	N = 6;
	T = buildSchurMatrix([
		[0, 0, 2],
		[0, 1, 3],
		[0, 2, 0.5],
		[0, 3, 0.3],
		[0, 4, 0.2],
		[0, 5, 0.1],
		[1, 0, -3],
		[1, 1, 2],
		[1, 2, 0.8],
		[1, 3, 0.4],
		[1, 4, 0.25],
		[1, 5, 0.15],
		[2, 2, 4],
		[2, 3, 1],
		[2, 4, 0.6],
		[2, 5, 0.35],
		[3, 2, -1],
		[3, 3, 4],
		[3, 4, 0.7],
		[3, 5, 0.45],
		[4, 4, 1],
		[4, 5, 0.9],
		[5, 5, 0.5]
	], N);
	Q = eye( N );
	WORK = new Float64Array( N );
	r = dtrexc( 'update', N, T, 1, N, 0, Q, 1, N, 0, 3, 1, WORK, 1, 0 );
	assert.strictEqual( r.info, tc.info, 'info' );
	assertArrayClose( toArray( T ), tc.T, 1e-12, 'T' );
	assertArrayClose( toArray( Q ), tc.Q, 1e-12, 'Q' );
});

test( 'dtrexc: ilst adjusted forward', function t() {
	var WORK;
	var tc;
	var N;
	var T;
	var Q;
	var r;

	tc = ilst_adjusted_fwd;
	N = 4;
	T = buildSchurMatrix([
		[0, 0, 5],
		[0, 1, 0.5],
		[0, 2, 0.2],
		[0, 3, 0.1],
		[1, 1, 3],
		[1, 2, 2],
		[1, 3, 0.6],
		[2, 1, -2],
		[2, 2, 3],
		[2, 3, 0.8],
		[3, 3, 1]
	], N);
	Q = eye( N );
	WORK = new Float64Array( N );
	r = dtrexc( 'update', N, T, 1, N, 0, Q, 1, N, 0, 1, 2, WORK, 1, 0 );
	assert.strictEqual( r.info, tc.info, 'info' );
	assertArrayClose( toArray( T ), tc.T, 1e-12, 'T' );
	assertArrayClose( toArray( Q ), tc.Q, 1e-12, 'Q' );
});

test( 'dtrexc: backward 2x2 block, COMPQ=N', function t() {
	var WORK;
	var tc;
	var N;
	var T;
	var Q;
	var r;

	tc = backward_2x2_compq_n;
	N = 5;
	T = buildSchurMatrix([
		[0, 0, 5],
		[0, 1, 0.5],
		[0, 2, 0.2],
		[0, 3, 0.1],
		[0, 4, 0.3],
		[1, 1, 1],
		[1, 2, 0.6],
		[1, 3, 0.4],
		[1, 4, 0.15],
		[2, 2, 0.5],
		[2, 3, 0.9],
		[2, 4, 0.2],
		[3, 3, 3],
		[3, 4, 2],
		[4, 3, -2],
		[4, 4, 3]
	], N);
	Q = eye( N );
	WORK = new Float64Array( N );
	r = dtrexc( 'none', N, T, 1, N, 0, Q, 1, N, 0, 4, 1, WORK, 1, 0 );
	assert.strictEqual( r.info, tc.info, 'info' );
	assertArrayClose( toArray( T ), tc.T, 1e-12, 'T' );
});
