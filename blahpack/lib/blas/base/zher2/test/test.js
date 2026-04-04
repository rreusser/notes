/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Complex128 = require( '@stdlib/complex/float64/ctor' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zher2 = require( './../lib/base.js' );
var ndarray = require( './../lib/ndarray.js' );

// FIXTURES //

var upper_basic = require( './fixtures/upper_basic.json' );
var lower_basic = require( './fixtures/lower_basic.json' );
var n_zero = require( './fixtures/n_zero.json' );
var alpha_zero = require( './fixtures/alpha_zero.json' );
var n_one = require( './fixtures/n_one.json' );
var upper_stride = require( './fixtures/upper_stride.json' );
var lower_stride = require( './fixtures/lower_stride.json' );
var complex_alpha = require( './fixtures/complex_alpha.json' );
var upper_3x3 = require( './fixtures/upper_3x3.json' );
var upper_zeros = require( './fixtures/upper_zeros.json' );

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

test( 'zher2: upper_basic (UPLO=U, N=2, alpha=(1,0))', function t() {
	var result;
	var alpha;
	var tc;
	var A;
	var x;
	var y;

	tc = upper_basic;
	A = new Complex128Array([
		2,
		0,
		0,
		0,
		1,
		1,
		3,
		0
	]);
	x = new Complex128Array( [ 1, 0, 0, 1 ] );
	y = new Complex128Array( [ 1, 1, 2, 0 ] );
	alpha = new Complex128( 1, 0 );
	result = zher2( 'upper', 2, alpha, x, 1, 0, y, 1, 0, A, 1, 2, 0 );
	assert.strictEqual( result, A );
	assertArrayClose( toArray( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zher2: lower_basic (UPLO=L, N=2, alpha=(1,0))', function t() {
	var result;
	var alpha;
	var tc;
	var A;
	var x;
	var y;

	tc = lower_basic;
	A = new Complex128Array([
		2,
		0,
		1,
		-1,
		0,
		0,
		3,
		0
	]);
	x = new Complex128Array( [ 1, 0, 0, 1 ] );
	y = new Complex128Array( [ 1, 1, 2, 0 ] );
	alpha = new Complex128( 1, 0 );
	result = zher2( 'lower', 2, alpha, x, 1, 0, y, 1, 0, A, 1, 2, 0 );
	assert.strictEqual( result, A );
	assertArrayClose( toArray( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zher2: n_zero (N=0 quick return)', function t() {
	var result;
	var alpha;
	var tc;
	var A;
	var x;
	var y;

	tc = n_zero;
	A = new Complex128Array( [ 99, 88 ] );
	x = new Complex128Array( 0 );
	y = new Complex128Array( 0 );
	alpha = new Complex128( 1, 0 );
	result = zher2( 'upper', 0, alpha, x, 1, 0, y, 1, 0, A, 1, 1, 0 );
	assert.strictEqual( result, A );
	assertArrayClose( toArray( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zher2: alpha_zero (alpha=0 quick return)', function t() {
	var result;
	var alpha;
	var tc;
	var Av;
	var A;
	var x;
	var y;

	tc = alpha_zero;
	A = new Complex128Array( [ 99, 88, 0, 0, 0, 0, 0, 0 ] );
	x = new Complex128Array( [ 1, 0, 0, 1 ] );
	y = new Complex128Array( [ 1, 1, 2, 0 ] );
	alpha = new Complex128( 0, 0 );
	result = zher2( 'upper', 2, alpha, x, 1, 0, y, 1, 0, A, 1, 2, 0 );
	assert.strictEqual( result, A );
	Av = toArray( reinterpret( A, 0 ) );
	assertArrayClose( Av.slice( 0, 2 ), tc.A, 1e-14, 'A' );
	assert.deepStrictEqual( Av.slice( 2 ), [ 0, 0, 0, 0, 0, 0 ] );
});

test( 'zher2: n_one (N=1)', function t() {
	var result;
	var alpha;
	var tc;
	var A;
	var x;
	var y;

	tc = n_one;
	A = new Complex128Array( [ 5, 0 ] );
	x = new Complex128Array( [ 2, 3 ] );
	y = new Complex128Array( [ 1, -1 ] );
	alpha = new Complex128( 1, 0 );
	result = zher2( 'upper', 1, alpha, x, 1, 0, y, 1, 0, A, 1, 1, 0 );
	assert.strictEqual( result, A );
	assertArrayClose( toArray( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zher2: upper_stride (UPLO=U, incx=2, incy=2)', function t() {
	var result;
	var alpha;
	var tc;
	var A;
	var x;
	var y;

	tc = upper_stride;
	A = new Complex128Array([
		2,
		0,
		0,
		0,
		0,
		0,
		1,
		1,
		3,
		0,
		0,
		0,
		2,
		-1,
		1,
		2,
		4,
		0
	]);
	x = new Complex128Array( [ 1, 0, 0, 0, 0, 1, 0, 0, 1, 1 ] );
	y = new Complex128Array( [ 1, 1, 0, 0, 2, 0, 0, 0, 0, -1 ] );
	alpha = new Complex128( 1, 0 );
	result = zher2( 'upper', 3, alpha, x, 2, 0, y, 2, 0, A, 1, 3, 0 );
	assert.strictEqual( result, A );
	assertArrayClose( toArray( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zher2: lower_stride (UPLO=L, incx=2, incy=2)', function t() {
	var result;
	var alpha;
	var tc;
	var A;
	var x;
	var y;

	tc = lower_stride;
	A = new Complex128Array([
		2,
		0,
		1,
		-1,
		2,
		1,
		0,
		0,
		3,
		0,
		1,
		-2,
		0,
		0,
		0,
		0,
		4,
		0
	]);
	x = new Complex128Array( [ 1, 0, 0, 0, 0, 1, 0, 0, 1, 1 ] );
	y = new Complex128Array( [ 1, 1, 0, 0, 2, 0, 0, 0, 0, -1 ] );
	alpha = new Complex128( 1, 0 );
	result = zher2( 'lower', 3, alpha, x, 2, 0, y, 2, 0, A, 1, 3, 0 );
	assert.strictEqual( result, A );
	assertArrayClose( toArray( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zher2: complex_alpha (alpha=(2,1))', function t() {
	var result;
	var alpha;
	var tc;
	var A;
	var x;
	var y;

	tc = complex_alpha;
	A = new Complex128Array([
		1,
		0,
		0,
		0,
		0,
		1,
		2,
		0
	]);
	x = new Complex128Array( [ 1, 0, 1, 0 ] );
	y = new Complex128Array( [ 1, 0, 0, 1 ] );
	alpha = new Complex128( 2, 1 );
	result = zher2( 'upper', 2, alpha, x, 1, 0, y, 1, 0, A, 1, 2, 0 );
	assert.strictEqual( result, A );
	assertArrayClose( toArray( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zher2: upper_3x3 (UPLO=U, N=3, larger matrix)', function t() {
	var result;
	var alpha;
	var tc;
	var A;
	var x;
	var y;

	tc = upper_3x3;
	A = new Complex128Array([
		1,
		0,
		0,
		0,
		0,
		0,
		2,
		1,
		3,
		0,
		0,
		0,
		0,
		-1,
		1,
		1,
		5,
		0
	]);
	x = new Complex128Array( [ 1, 1, 2, 0, 0, 3 ] );
	y = new Complex128Array( [ 0, 1, 1, -1, 2, 2 ] );
	alpha = new Complex128( 1, 0 );
	result = zher2( 'upper', 3, alpha, x, 1, 0, y, 1, 0, A, 1, 3, 0 );
	assert.strictEqual( result, A );
	assertArrayClose( toArray( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zher2: lower_zeros (UPLO=L, x and y with zero elements — tests skip branch)', function t() { // eslint-disable-line max-len
	var result;
	var alpha;
	var Av;
	var A;
	var x;
	var y;

	A = new Complex128Array([
		1,
		0.5,
		2,
		1,
		0,
		0,
		3,
		0.5
	]);
	x = new Complex128Array( [ 0, 0, 1, 0 ] );
	y = new Complex128Array( [ 0, 0, 2, 0 ] );
	alpha = new Complex128( 1, 0 );
	result = zher2( 'lower', 2, alpha, x, 1, 0, y, 1, 0, A, 1, 2, 0 );
	assert.strictEqual( result, A );
	Av = toArray( reinterpret( A, 0 ) );
	assert.strictEqual( Av[ 0 ], 1.0, 'A(0,0) real unchanged' );
	assert.strictEqual( Av[ 1 ], 0.0, 'A(0,0) imag forced to 0' );
	assert.strictEqual( Av[ 6 ], 7.0, 'A(1,1) updated' );
	assert.strictEqual( Av[ 7 ], 0.0, 'A(1,1) imag is 0' );
});

test( 'zher2: upper_zeros (x and y with zero elements — tests skip branch)', function t() { // eslint-disable-line max-len
	var result;
	var alpha;
	var tc;
	var A;
	var x;
	var y;

	tc = upper_zeros;
	A = new Complex128Array([
		1,
		0,
		0,
		0,
		2,
		1,
		3,
		0
	]);
	x = new Complex128Array( [ 0, 0, 1, 0 ] );
	y = new Complex128Array( [ 0, 0, 2, 0 ] );
	alpha = new Complex128( 1, 0 );
	result = zher2( 'upper', 2, alpha, x, 1, 0, y, 1, 0, A, 1, 2, 0 );
	assert.strictEqual( result, A );
	assertArrayClose( toArray( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

// ndarray validation tests

test( 'zher2: ndarray throws TypeError for invalid uplo', function t() {
	var alpha = new Complex128( 1, 0 );
	assert.throws( function throws() {
		ndarray( 'invalid', 2, alpha, new Complex128Array( 2 ), 1, 0, new Complex128Array( 2 ), 1, 0, new Complex128Array( 4 ), 1, 2, 0 ); // eslint-disable-line max-len
	}, TypeError );
});

test( 'zher2: ndarray throws RangeError for negative N', function t() {
	var alpha = new Complex128( 1, 0 );
	assert.throws( function throws() {
		ndarray( 'upper', -1, alpha, new Complex128Array( 2 ), 1, 0, new Complex128Array( 2 ), 1, 0, new Complex128Array( 4 ), 1, 2, 0 ); // eslint-disable-line max-len
	}, RangeError );
});

test( 'zher2: ndarray throws RangeError for zero strideX', function t() {
	var alpha = new Complex128( 1, 0 );
	assert.throws( function throws() {
		ndarray( 'upper', 2, alpha, new Complex128Array( 2 ), 0, 0, new Complex128Array( 2 ), 1, 0, new Complex128Array( 4 ), 1, 2, 0 ); // eslint-disable-line max-len
	}, RangeError );
});

test( 'zher2: ndarray throws RangeError for zero strideY', function t() {
	var alpha = new Complex128( 1, 0 );
	assert.throws( function throws() {
		ndarray( 'upper', 2, alpha, new Complex128Array( 2 ), 1, 0, new Complex128Array( 2 ), 0, 0, new Complex128Array( 4 ), 1, 2, 0 ); // eslint-disable-line max-len
	}, RangeError );
});
