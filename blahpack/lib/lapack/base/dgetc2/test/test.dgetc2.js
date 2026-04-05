/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dgetc2 = require( './../lib/dgetc2.js' );


// TESTS //

test( 'dgetc2 is a function', function t() {
	assert.strictEqual( typeof dgetc2, 'function', 'is a function' );
});

test( 'dgetc2 has expected arity', function t() {
	assert.strictEqual( dgetc2.length, 7, 'has expected arity' );
});

test( 'dgetc2 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dgetc2( -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
