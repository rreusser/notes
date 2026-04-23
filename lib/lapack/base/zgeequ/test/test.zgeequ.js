/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zgeequ = require( './../lib/zgeequ.js' );


// TESTS //

test( 'zgeequ is a function', function t() {
	assert.strictEqual( typeof zgeequ, 'function', 'is a function' );
});

test( 'zgeequ has expected arity', function t() {
	assert.strictEqual( zgeequ.length, 2, 'has expected arity' );
});
