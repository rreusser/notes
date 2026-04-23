/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlaqr1 = require( './../lib/dlaqr1.js' );


// TESTS //

test( 'dlaqr1 is a function', function t() {
	assert.strictEqual( typeof dlaqr1, 'function', 'is a function' );
});

test( 'dlaqr1 has expected arity', function t() {
	assert.strictEqual( dlaqr1.length, 9, 'has expected arity' );
});

test( 'dlaqr1 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlaqr1( -1, new Float64Array( 4 ), 2, 2, 2, 2, 2, 2, 1 );
	}, RangeError );
});
