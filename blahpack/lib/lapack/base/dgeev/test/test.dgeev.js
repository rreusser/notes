/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dgeev = require( './../lib/dgeev.js' );


// TESTS //

test( 'dgeev is a function', function t() {
	assert.strictEqual( typeof dgeev, 'function', 'is a function' );
});

test( 'dgeev has expected arity', function t() {
	assert.strictEqual( dgeev.length, 13, 'has expected arity' );
});

test( 'dgeev throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dgeev( 2, 2, -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});
