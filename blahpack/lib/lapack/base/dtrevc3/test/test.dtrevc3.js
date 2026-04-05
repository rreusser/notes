/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dtrevc3 = require( './../lib/dtrevc3.js' );


// TESTS //

test( 'dtrevc3 is a function', function t() {
	assert.strictEqual( typeof dtrevc3, 'function', 'is a function' );
});

test( 'dtrevc3 has expected arity', function t() {
	assert.strictEqual( dtrevc3.length, 16, 'has expected arity' );
});

test( 'dtrevc3 throws TypeError for invalid side', function t() {
	assert.throws( function throws() {
		dtrevc3( 'invalid', 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), new Float64Array( 4 ), 1, 2 );
	}, TypeError );
});

test( 'dtrevc3 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dtrevc3( 'left', 2, new Float64Array( 4 ), 1, -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), new Float64Array( 4 ), 1, 2 );
	}, RangeError );
});

test( 'dtrevc3 throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dtrevc3( 'left', 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, -1, new Float64Array( 4 ), 1, 2 );
	}, RangeError );
});
