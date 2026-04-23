/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlahqr = require( './../lib/dlahqr.js' );


// TESTS //

test( 'dlahqr is a function', function t() {
	assert.strictEqual( typeof dlahqr, 'function', 'is a function' );
});

test( 'dlahqr has expected arity', function t() {
	assert.strictEqual( dlahqr.length, 15, 'has expected arity' );
});

test( 'dlahqr throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlahqr( 2, 2, -1, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, 2, 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});
