/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zdotc = require( './../lib/zdotc.js' );


// TESTS //

test( 'zdotc is a function', function t() {
	assert.strictEqual( typeof zdotc, 'function', 'is a function' );
});

test( 'zdotc has expected arity', function t() {
	assert.strictEqual( zdotc.length, 5, 'has expected arity' );
});

test( 'zdotc throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zdotc( -1, 2, 1, 2, 1 );
	}, RangeError );
});
