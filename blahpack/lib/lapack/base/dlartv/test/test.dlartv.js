/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlartv = require( './../lib/dlartv.js' );


// TESTS //

test( 'dlartv is a function', function t() {
	assert.strictEqual( typeof dlartv, 'function', 'is a function' );
});

test( 'dlartv has expected arity', function t() {
	assert.strictEqual( dlartv.length, 8, 'has expected arity' );
});

test( 'dlartv throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlartv( -1, 2, 1, 2, 1, 2, 2, 1 );
	}, RangeError );
});
