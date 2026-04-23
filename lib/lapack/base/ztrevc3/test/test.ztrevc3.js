/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var ztrevc3 = require( './../lib/ztrevc3.js' );


// TESTS //

test( 'ztrevc3 is a function', function t() {
	assert.strictEqual( typeof ztrevc3, 'function', 'is a function' );
});

test( 'ztrevc3 has expected arity', function t() {
	assert.strictEqual( ztrevc3.length, 2, 'has expected arity' );
});
