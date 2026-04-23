/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlaed2 = require( './../lib/dlaed2.js' );


// TESTS //

test( 'dlaed2 is a function', function t() {
	assert.strictEqual( typeof dlaed2, 'function', 'is a function' );
});

test( 'dlaed2 has expected arity', function t() {
	assert.strictEqual( dlaed2.length, 15, 'has expected arity' );
});

test( 'dlaed2 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlaed2( -1, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ) );
	}, RangeError );
});
