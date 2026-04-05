/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dsterf = require( './../lib/dsterf.js' );


// TESTS //

test( 'dsterf is a function', function t() {
	assert.strictEqual( typeof dsterf, 'function', 'is a function' );
});

test( 'dsterf has expected arity', function t() {
	assert.strictEqual( dsterf.length, 5, 'has expected arity' );
});

test( 'dsterf throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dsterf( -1, 2, 1, 2, 1 );
	}, RangeError );
});
