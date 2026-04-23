/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dgttrf = require( './../lib/dgttrf.js' );


// TESTS //

test( 'dgttrf is a function', function t() {
	assert.strictEqual( typeof dgttrf, 'function', 'is a function' );
});

test( 'dgttrf has expected arity', function t() {
	assert.strictEqual( dgttrf.length, 11, 'has expected arity' );
});

test( 'dgttrf throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dgttrf( -1, new Float64Array( 4 ), 1, 2, 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
