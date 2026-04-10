

/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlarrd = require( './../lib/dlarrd.js' );


// TESTS //

test( 'dlarrd is a function', function t() {
	assert.strictEqual( typeof dlarrd, 'function', 'is a function' );
});

test( 'dlarrd has expected arity', function t() {
	assert.strictEqual( dlarrd.length, 39, 'has expected arity' );
});

test( 'dlarrd throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dlarrd( 'no-transpose', 'invalid', 2, 2, 2, 2, 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 2, 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 2, 2, 2, 2, 2, 2, new Float64Array( 4 ), 2, 2, 2, 2 );
	}, TypeError );
});

test( 'dlarrd throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlarrd( 'no-transpose', 'row-major', -1, 2, 2, 2, 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 2, 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 2, 2, 2, 2, 2, 2, new Float64Array( 4 ), 2, 2, 2, 2 );
	}, RangeError );
});

test( 'dlarrd throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dlarrd( 'no-transpose', 'row-major', 2, 2, 2, 2, 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 2, 2, 2, -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 2, 2, 2, 2, 2, 2, new Float64Array( 4 ), 2, 2, 2, 2 );
	}, RangeError );
});

