/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlaebz = require( './../lib/dlaebz.js' );


// TESTS //

test( 'dlaebz is a function', function t() {
	assert.strictEqual( typeof dlaebz, 'function', 'is a function' );
});

test( 'dlaebz has expected arity', function t() {
	assert.strictEqual( dlaebz.length, 28, 'has expected arity' );
});

test( 'dlaebz throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlaebz( 2, 2, -1, 2, 2, 2, 2, 2, 2, 2, 1, 2, 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, 2, 1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
