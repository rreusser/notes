/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zgeqr2 = require( './../lib/zgeqr2.js' );


// TESTS //

test( 'zgeqr2 is a function', function t() {
	assert.strictEqual( typeof zgeqr2, 'function', 'is a function' );
});

test( 'zgeqr2 has expected arity', function t() {
	assert.strictEqual( zgeqr2.length, 9, 'has expected arity' );
});

test( 'zgeqr2 throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zgeqr2( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zgeqr2 throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		zgeqr2( 'row-major', -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'zgeqr2 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zgeqr2( 'row-major', new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
