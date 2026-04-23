/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zpocon = require( './../lib/zpocon.js' );


// TESTS //

test( 'zpocon is a function', function t() {
	assert.strictEqual( typeof zpocon, 'function', 'is a function' );
});

test( 'zpocon has expected arity', function t() {
	assert.strictEqual( zpocon.length, 2, 'has expected arity' );
});
