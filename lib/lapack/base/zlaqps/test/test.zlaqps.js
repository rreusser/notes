/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlaqps = require( './../lib/zlaqps.js' );


// TESTS //

test( 'zlaqps is a function', function t() {
	assert.strictEqual( typeof zlaqps, 'function', 'is a function' );
});

test( 'zlaqps has expected arity', function t() {
	assert.strictEqual( zlaqps.length, 19, 'has expected arity' );
});

test( 'zlaqps throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zlaqps( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zlaqps throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		zlaqps( 'row-major', -1, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'zlaqps throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zlaqps( 'row-major', new Float64Array( 4 ), -1, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2 );
	}, RangeError );
});
