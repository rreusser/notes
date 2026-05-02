
/* eslint-disable no-restricted-syntax, stdlib/first-unit-test, max-len */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var zgbbrd = require( './../lib/zgbbrd.js' );


// TESTS //

test( 'zgbbrd is a function', function t() {
	assert.strictEqual( typeof zgbbrd, 'function', 'is a function' );
});

test( 'zgbbrd has expected arity', function t() {
	assert.strictEqual( zgbbrd.length, 23, 'has expected arity' );
});

test( 'zgbbrd throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zgbbrd( 'invalid', 'no-vectors', 2, 2, 0, 1, 1, new Complex128Array( 12 ), 3, new Float64Array( 2 ), 1, new Float64Array( 1 ), 1, new Complex128Array( 1 ), 1, new Complex128Array( 1 ), 1, new Complex128Array( 1 ), 1, new Complex128Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zgbbrd throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		zgbbrd( 'column-major', 'no-vectors', -1, 2, 0, 1, 1, new Complex128Array( 12 ), 3, new Float64Array( 2 ), 1, new Float64Array( 1 ), 1, new Complex128Array( 1 ), 1, new Complex128Array( 1 ), 1, new Complex128Array( 1 ), 1, new Complex128Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'zgbbrd throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zgbbrd( 'column-major', 'no-vectors', 2, -1, 0, 1, 1, new Complex128Array( 12 ), 3, new Float64Array( 2 ), 1, new Float64Array( 1 ), 1, new Complex128Array( 1 ), 1, new Complex128Array( 1 ), 1, new Complex128Array( 1 ), 1, new Complex128Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'zgbbrd throws RangeError for LDAB < KL+KU+1', function t() {
	assert.throws( function throws() {
		zgbbrd( 'column-major', 'no-vectors', 2, 2, 0, 1, 1, new Complex128Array( 12 ), 1, new Float64Array( 2 ), 1, new Float64Array( 1 ), 1, new Complex128Array( 1 ), 1, new Complex128Array( 1 ), 1, new Complex128Array( 1 ), 1, new Complex128Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'zgbbrd column-major path executes', function t() {
	var ldab = 3;
	var AB = new Complex128Array( ldab * 3 );
	var d = new Float64Array( 3 );
	var e = new Float64Array( 2 );
	var Q = new Complex128Array( 9 );
	var PT = new Complex128Array( 9 );
	var C = new Complex128Array( 9 );
	var WORK = new Complex128Array( 6 );
	var RWORK = new Float64Array( 6 );
	var info = zgbbrd( 'column-major', 'no-vectors', 3, 3, 0, 1, 1, AB, 3, d, 1, e, 1, Q, 3, PT, 3, C, 3, WORK, 1, RWORK, 1 );
	assert.strictEqual( info, 0 );
});

test( 'zgbbrd row-major path executes', function t() {
	var ldab = 3;
	var AB = new Complex128Array( ldab * 3 );
	var d = new Float64Array( 3 );
	var e = new Float64Array( 2 );
	var Q = new Complex128Array( 9 );
	var PT = new Complex128Array( 9 );
	var C = new Complex128Array( 9 );
	var WORK = new Complex128Array( 6 );
	var RWORK = new Float64Array( 6 );
	var info = zgbbrd( 'row-major', 'no-vectors', 3, 3, 0, 1, 1, AB, 3, d, 1, e, 1, Q, 3, PT, 3, C, 3, WORK, 1, RWORK, 1 );
	assert.strictEqual( info, 0 );
});

test( 'zgbbrd exposes ndarray method', function t() {
	var main = require( './../lib' );
	assert.strictEqual( typeof main.ndarray, 'function', 'has ndarray method' );
});
