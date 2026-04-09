
/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dhgeqz = require( './../lib/dhgeqz.js' );


// TESTS //

test( 'dhgeqz is a function', function t() {
	assert.strictEqual( typeof dhgeqz, 'function', 'is a function' );
});

test( 'dhgeqz has expected arity', function t() {
	assert.strictEqual( dhgeqz.length, 24, 'has expected arity' );
});

test( 'dhgeqz throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dhgeqz( 'invalid', 'no-transpose', 'no-transpose', 'no-transpose', 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2 );
	}, TypeError );
});

test( 'dhgeqz throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dhgeqz( 'row-major', 'no-transpose', 'no-transpose', 'no-transpose', -1, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2 );
	}, RangeError );
});
