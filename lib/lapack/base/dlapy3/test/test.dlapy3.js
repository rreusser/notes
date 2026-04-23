/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlapy3 = require( './../lib/dlapy3.js' );


// TESTS //

test( 'dlapy3 is a function', function t() {
	assert.strictEqual( typeof dlapy3, 'function', 'is a function' );
});

test( 'dlapy3 has expected arity', function t() {
	assert.strictEqual( dlapy3.length, 3, 'has expected arity' );
});
