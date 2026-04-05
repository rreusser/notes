/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlarscl2 = require( './../lib/zlarscl2.js' );


// TESTS //

test( 'zlarscl2 is a function', function t() {
	assert.strictEqual( typeof zlarscl2, 'function', 'is a function' );
});

test( 'zlarscl2 has expected arity', function t() {
	assert.strictEqual( zlarscl2.length, 6, 'has expected arity' );
});

test( 'zlarscl2 throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zlarscl2( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zlarscl2 throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		zlarscl2( 'row-major', -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'zlarscl2 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zlarscl2( 'row-major', new Float64Array( 4 ), -1, 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});
