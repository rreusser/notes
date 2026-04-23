/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dgeqp3 = require( './../lib/dgeqp3.js' );


// TESTS //

test( 'dgeqp3 is a function', function t() {
	assert.strictEqual( typeof dgeqp3, 'function', 'is a function' );
});

test( 'dgeqp3 has expected arity', function t() {
	assert.strictEqual( dgeqp3.length, 8, 'has expected arity' );
});

test( 'dgeqp3 throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dgeqp3( -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'dgeqp3 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dgeqp3( new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
