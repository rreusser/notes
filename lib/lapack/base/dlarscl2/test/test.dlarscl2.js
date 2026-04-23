/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlarscl2 = require( './../lib/dlarscl2.js' );


// TESTS //

test( 'dlarscl2 is a function', function t() {
	assert.strictEqual( typeof dlarscl2, 'function', 'is a function' );
});

test( 'dlarscl2 has expected arity', function t() {
	assert.strictEqual( dlarscl2.length, 6, 'has expected arity' );
});

test( 'dlarscl2 throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dlarscl2( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dlarscl2 throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dlarscl2( 'row-major', -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'dlarscl2 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlarscl2( 'row-major', new Float64Array( 4 ), -1, 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});
