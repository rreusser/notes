/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlasdt = require( './../lib/dlasdt.js' );


// TESTS //

test( 'dlasdt is a function', function t() {
	assert.strictEqual( typeof dlasdt, 'function', 'is a function' );
});

test( 'dlasdt has expected arity', function t() {
	assert.strictEqual( dlasdt.length, 10, 'has expected arity' );
});

test( 'dlasdt throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlasdt( -1, 2, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, 2 );
	}, RangeError );
});
