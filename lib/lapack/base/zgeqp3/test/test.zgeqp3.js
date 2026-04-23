/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zgeqp3 = require( './../lib/zgeqp3.js' );


// TESTS //

test( 'zgeqp3 is a function', function t() {
	assert.strictEqual( typeof zgeqp3, 'function', 'is a function' );
});

test( 'zgeqp3 has expected arity', function t() {
	assert.strictEqual( zgeqp3.length, 14, 'has expected arity' );
});

test( 'zgeqp3 throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zgeqp3( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zgeqp3 throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		zgeqp3( 'row-major', -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'zgeqp3 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zgeqp3( 'row-major', new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});
