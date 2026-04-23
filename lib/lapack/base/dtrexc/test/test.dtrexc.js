/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dtrexc = require( './../lib/dtrexc.js' );


// TESTS //

test( 'dtrexc is a function', function t() {
	assert.strictEqual( typeof dtrexc, 'function', 'is a function' );
});

test( 'dtrexc has expected arity', function t() {
	assert.strictEqual( dtrexc.length, 10, 'has expected arity' );
});

test( 'dtrexc throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dtrexc( 2, -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});
