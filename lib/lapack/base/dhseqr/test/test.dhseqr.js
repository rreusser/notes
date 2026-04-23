/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dhseqr = require( './../lib/dhseqr.js' );


// TESTS //

test( 'dhseqr is a function', function t() {
	assert.strictEqual( typeof dhseqr, 'function', 'is a function' );
});

test( 'dhseqr has expected arity', function t() {
	assert.strictEqual( dhseqr.length, 13, 'has expected arity' );
});

test( 'dhseqr throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dhseqr( 2, 2, -1, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2 );
	}, RangeError );
});
