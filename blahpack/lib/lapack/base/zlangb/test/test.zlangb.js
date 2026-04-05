/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlangb = require( './../lib/zlangb.js' );


// TESTS //

test( 'zlangb is a function', function t() {
	assert.strictEqual( typeof zlangb, 'function', 'is a function' );
});

test( 'zlangb has expected arity', function t() {
	assert.strictEqual( zlangb.length, 8, 'has expected arity' );
});

test( 'zlangb throws TypeError for invalid norm', function t() {
	assert.throws( function throws() {
		zlangb( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zlangb throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zlangb( 'max', -1, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});
