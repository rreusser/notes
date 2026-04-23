/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var drot = require( './../lib/drot.js' );


// TESTS //

test( 'drot is a function', function t() {
	assert.strictEqual( typeof drot, 'function', 'is a function' );
});

test( 'drot has expected arity', function t() {
	assert.strictEqual( drot.length, 7, 'has expected arity' );
});

test( 'drot throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		drot( -1, 2, 1, 2, 1, 2, 2 );
	}, RangeError );
});
