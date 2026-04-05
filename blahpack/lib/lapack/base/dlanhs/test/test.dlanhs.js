/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlanhs = require( './../lib/dlanhs.js' );


// TESTS //

test( 'dlanhs is a function', function t() {
	assert.strictEqual( typeof dlanhs, 'function', 'is a function' );
});

test( 'dlanhs has expected arity', function t() {
	assert.strictEqual( dlanhs.length, 7, 'has expected arity' );
});

test( 'dlanhs throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dlanhs( 'invalid', 2, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dlanhs throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlanhs( 'row-major', 2, -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});
