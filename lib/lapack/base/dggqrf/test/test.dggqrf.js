/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dggqrf = require( './../lib/dggqrf.js' );


// TESTS //

test( 'dggqrf is a function', function t() {
	assert.strictEqual( typeof dggqrf, 'function', 'is a function' );
});

test( 'dggqrf has expected arity', function t() {
	assert.strictEqual( dggqrf.length, 14, 'has expected arity' );
});

test( 'dggqrf throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dggqrf( -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, 2 );
	}, RangeError );
});

test( 'dggqrf throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dggqrf( new Float64Array( 4 ), -1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, 2 );
	}, RangeError );
});
