/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlargv = require( './../lib/dlargv.js' );


// TESTS //

test( 'dlargv is a function', function t() {
	assert.strictEqual( typeof dlargv, 'function', 'is a function' );
});

test( 'dlargv has expected arity', function t() {
	assert.strictEqual( dlargv.length, 7, 'has expected arity' );
});

test( 'dlargv throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlargv( -1, 2, 1, 2, 1, 2, 1 );
	}, RangeError );
});
