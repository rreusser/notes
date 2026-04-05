/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlaqp2 = require( './../lib/zlaqp2.js' );


// TESTS //

test( 'zlaqp2 is a function', function t() {
	assert.strictEqual( typeof zlaqp2, 'function', 'is a function' );
});

test( 'zlaqp2 has expected arity', function t() {
	assert.strictEqual( zlaqp2.length, 16, 'has expected arity' );
});

test( 'zlaqp2 throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zlaqp2( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zlaqp2 throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		zlaqp2( 'row-major', -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'zlaqp2 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zlaqp2( 'row-major', new Float64Array( 4 ), -1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
