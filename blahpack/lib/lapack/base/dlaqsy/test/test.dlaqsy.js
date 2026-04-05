/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlaqsy = require( './../lib/dlaqsy.js' );


// TESTS //

test( 'dlaqsy is a function', function t() {
	assert.strictEqual( typeof dlaqsy, 'function', 'is a function' );
});

test( 'dlaqsy has expected arity', function t() {
	assert.strictEqual( dlaqsy.length, 8, 'has expected arity' );
});

test( 'dlaqsy throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dlaqsy( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 1, 2, 2 );
	}, TypeError );
});

test( 'dlaqsy throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlaqsy( 'upper', -1, new Float64Array( 4 ), 2, 2, 1, 2, 2 );
	}, RangeError );
});
