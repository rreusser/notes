

/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dtgsen = require( './../lib/dtgsen.js' );


// TESTS //

test( 'dtgsen is a function', function t() {
	assert.strictEqual( typeof dtgsen, 'function', 'is a function' );
});

test( 'dtgsen has expected arity', function t() {
	assert.strictEqual( dtgsen.length, 33, 'has expected arity' );
});

test( 'dtgsen throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dtgsen( 'invalid', 2, 2, 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 2, 2, 2 );
	}, TypeError );
});

test( 'dtgsen throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dtgsen( 'row-major', 2, 2, 2, new Float64Array( 4 ), 2, -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 2, 2, 2 );
	}, RangeError );
});

test( 'dtgsen throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dtgsen( 'row-major', 2, 2, 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, -1, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 2, 2, 2 );
	}, RangeError );
});

