/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlarfg = require( './../lib/dlarfg.js' );


// TESTS //

test( 'dlarfg is a function', function t() {
	assert.strictEqual( typeof dlarfg, 'function', 'is a function' );
});

test( 'dlarfg has expected arity', function t() {
	assert.strictEqual( dlarfg.length, 7, 'has expected arity' );
});

test( 'dlarfg throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlarfg( -1, 2, 2, 2, 1, 2, 2 );
	}, RangeError );
});
