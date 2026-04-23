/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dggbal = require( './../lib/dggbal.js' );


// TESTS //

test( 'dggbal is a function', function t() {
	assert.strictEqual( typeof dggbal, 'function', 'is a function' );
});

test( 'dggbal has expected arity', function t() {
	assert.strictEqual( dggbal.length, 13, 'has expected arity' );
});

test( 'dggbal throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dggbal( 'invalid', 2, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dggbal throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dggbal( 'row-major', 2, -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
