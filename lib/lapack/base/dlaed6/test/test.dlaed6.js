/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlaed6 = require( './../lib/dlaed6.js' );


// TESTS //

test( 'dlaed6 is a function', function t() {
	assert.strictEqual( typeof dlaed6, 'function', 'is a function' );
});

test( 'dlaed6 has expected arity', function t() {
	assert.strictEqual( dlaed6.length, 7, 'has expected arity' );
});
