/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dggsvd3 = require( './../lib/dggsvd3.js' );


// TESTS //

test( 'dggsvd3 is a function', function t() {
	assert.strictEqual( typeof dggsvd3, 'function', 'is a function' );
});

test( 'dggsvd3 has expected arity', function t() {
	assert.strictEqual( dggsvd3.length, 27, 'has expected arity' );
});

test( 'dggsvd3 throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dggsvd3( 2, 2, 2, -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'dggsvd3 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dggsvd3( 2, 2, 2, new Float64Array( 4 ), -1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'dggsvd3 throws RangeError for negative K', function t() {
	assert.throws( function throws() {
		dggsvd3( 2, 2, 2, new Float64Array( 4 ), new Float64Array( 4 ), 2, -1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});
