/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlapy2 = require( './../lib/dlapy2.js' );


// TESTS //

test( 'dlapy2 is a function', function t() {
	assert.strictEqual( typeof dlapy2, 'function', 'is a function' );
});

test( 'dlapy2 has expected arity', function t() {
	assert.strictEqual( dlapy2.length, 2, 'has expected arity' );
});
