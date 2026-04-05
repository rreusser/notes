/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlaesy = require( './../lib/zlaesy.js' );


// TESTS //

test( 'zlaesy is a function', function t() {
	assert.strictEqual( typeof zlaesy, 'function', 'is a function' );
});

test( 'zlaesy has expected arity', function t() {
	assert.strictEqual( zlaesy.length, 3, 'has expected arity' );
});
