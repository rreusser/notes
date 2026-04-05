/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlaruv = require( './../lib/dlaruv.js' );


// TESTS //

test( 'dlaruv is a function', function t() {
	assert.strictEqual( typeof dlaruv, 'function', 'is a function' );
});

test( 'dlaruv has expected arity', function t() {
	assert.strictEqual( dlaruv.length, 5, 'has expected arity' );
});

test( 'dlaruv throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlaruv( 2, 1, -1, 2, 1 );
	}, RangeError );
});
