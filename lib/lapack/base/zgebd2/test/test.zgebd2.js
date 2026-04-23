/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zgebd2 = require( './../lib/zgebd2.js' );


// TESTS //

test( 'zgebd2 is a function', function t() {
	assert.strictEqual( typeof zgebd2, 'function', 'is a function' );
});

test( 'zgebd2 has expected arity', function t() {
	assert.strictEqual( zgebd2.length, 15, 'has expected arity' );
});

test( 'zgebd2 throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zgebd2( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 1, 2, 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zgebd2 throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		zgebd2( 'row-major', -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 1, 2, 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'zgebd2 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zgebd2( 'row-major', new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, 2, 1, 2, 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
