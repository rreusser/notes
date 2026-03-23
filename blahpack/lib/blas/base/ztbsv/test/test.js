

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var ztbsv = require( './../lib' );
var ndarray = require( './../lib/ndarray.js' );

test( 'ztbsv: main export is a function', function t() {
	assert.strictEqual( typeof ztbsv, 'function' );
});

test( 'ztbsv: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof ztbsv.ndarray, 'function' );
});


// NDARRAY VALIDATION TESTS //

test( 'ndarray: throws TypeError for invalid uplo', function t() {
	var AB = new Complex128Array( 4 );
	var x = new Complex128Array( [ 1, 0, 1, 1 ] );
	assert.throws( function f() {
		ndarray( 'foo', 'no-transpose', 'non-unit', 2, 1, AB, 1, 2, 0, x, 1, 0 );
	}, TypeError );
});

test( 'ndarray: throws TypeError for invalid trans', function t() {
	var AB = new Complex128Array( 4 );
	var x = new Complex128Array( [ 1, 0, 1, 1 ] );
	assert.throws( function f() {
		ndarray( 'upper', 'foo', 'non-unit', 2, 1, AB, 1, 2, 0, x, 1, 0 );
	}, TypeError );
});

test( 'ndarray: throws TypeError for invalid diag', function t() {
	var AB = new Complex128Array( 4 );
	var x = new Complex128Array( [ 1, 0, 1, 1 ] );
	assert.throws( function f() {
		ndarray( 'upper', 'no-transpose', 'foo', 2, 1, AB, 1, 2, 0, x, 1, 0 );
	}, TypeError );
});

test( 'ndarray: throws RangeError for negative N', function t() {
	var AB = new Complex128Array( 4 );
	var x = new Complex128Array( [ 1, 0, 1, 1 ] );
	assert.throws( function f() {
		ndarray( 'upper', 'no-transpose', 'non-unit', -1, 1, AB, 1, 2, 0, x, 1, 0 );
	}, RangeError );
});

test( 'ndarray: throws RangeError for negative K', function t() {
	var AB = new Complex128Array( 4 );
	var x = new Complex128Array( [ 1, 0, 1, 1 ] );
	assert.throws( function f() {
		ndarray( 'upper', 'no-transpose', 'non-unit', 2, -1, AB, 1, 2, 0, x, 1, 0 );
	}, RangeError );
});

test( 'ndarray: throws RangeError for strideX=0', function t() {
	var AB = new Complex128Array( 4 );
	var x = new Complex128Array( [ 1, 0, 1, 1 ] );
	assert.throws( function f() {
		ndarray( 'upper', 'no-transpose', 'non-unit', 2, 1, AB, 1, 2, 0, x, 0, 0 );
	}, RangeError );
});

test( 'ndarray: N=0 early return', function t() {
	var AB = new Complex128Array( 1 );
	var x = new Complex128Array( [ 5, 5 ] );
	var out = ndarray( 'upper', 'no-transpose', 'non-unit', 0, 0, AB, 1, 1, 0, x, 1, 0 );
	assert.strictEqual( out, x );
	var xv = reinterpret( x, 0 );
	assert.strictEqual( xv[ 0 ], 5 );
	assert.strictEqual( xv[ 1 ], 5 );
});
