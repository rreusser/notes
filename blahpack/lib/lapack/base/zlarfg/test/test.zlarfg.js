/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlarfg = require( './../lib/zlarfg.js' );


// TESTS //

test( 'zlarfg is a function', function t() {
	assert.strictEqual( typeof zlarfg, 'function', 'is a function' );
});

test( 'zlarfg has expected arity', function t() {
	assert.strictEqual( zlarfg.length, 7, 'has expected arity' );
});

test( 'zlarfg throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zlarfg( -1, 2, 2, 2, 1, 2, 2 );
	}, RangeError );
});
