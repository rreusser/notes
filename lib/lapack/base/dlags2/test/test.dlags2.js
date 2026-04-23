/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlags2 = require( './../lib/dlags2.js' );


// TESTS //

test( 'dlags2 is a function', function t() {
	assert.strictEqual( typeof dlags2, 'function', 'is a function' );
});

test( 'dlags2 has expected arity', function t() {
	assert.strictEqual( dlags2.length, 7, 'has expected arity' );
});
