/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlasd7 = require( './../lib/dlasd7.js' );


// TESTS //

test( 'dlasd7 is a function', function t() {
	assert.strictEqual( typeof dlasd7, 'function', 'is a function' );
});

test( 'dlasd7 has expected arity', function t() {
	assert.strictEqual( dlasd7.length, 22, 'has expected arity' );
});
