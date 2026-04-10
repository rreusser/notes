
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
