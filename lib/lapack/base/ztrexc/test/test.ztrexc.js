/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var ztrexc = require( './../lib/ztrexc.js' );


// TESTS //

test( 'ztrexc is a function', function t() {
	assert.strictEqual( typeof ztrexc, 'function', 'is a function' );
});

test( 'ztrexc has expected arity', function t() {
	assert.strictEqual( ztrexc.length, 8, 'has expected arity' );
});

test( 'ztrexc throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		ztrexc( 2, -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2 );
	}, RangeError );
});
