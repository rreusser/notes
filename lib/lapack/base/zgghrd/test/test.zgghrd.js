/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zgghrd = require( './../lib/zgghrd.js' );


// TESTS //

test( 'zgghrd is a function', function t() {
	assert.strictEqual( typeof zgghrd, 'function', 'is a function' );
});

test( 'zgghrd has expected arity', function t() {
	assert.strictEqual( zgghrd.length, 14, 'has expected arity' );
});

test( 'zgghrd throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zgghrd( 'invalid', 2, 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zgghrd throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zgghrd( 'row-major', 2, 2, -1, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});
