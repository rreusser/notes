/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dtgex2 = require( './../lib/dtgex2.js' );


// TESTS //

test( 'dtgex2 is a function', function t() {
	assert.strictEqual( typeof dtgex2, 'function', 'is a function' );
});

test( 'dtgex2 has expected arity', function t() {
	assert.strictEqual( dtgex2.length, 18, 'has expected arity' );
});

test( 'dtgex2 throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dtgex2( 'invalid', 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 2, new Float64Array( 4 ), 2, 2 );
	}, TypeError );
});

test( 'dtgex2 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dtgex2( 'row-major', 2, 2, -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 2, new Float64Array( 4 ), 2, 2 );
	}, RangeError );
});
