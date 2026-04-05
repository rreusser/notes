/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dgghrd = require( './../lib/dgghrd.js' );


// TESTS //

test( 'dgghrd is a function', function t() {
	assert.strictEqual( typeof dgghrd, 'function', 'is a function' );
});

test( 'dgghrd has expected arity', function t() {
	assert.strictEqual( dgghrd.length, 14, 'has expected arity' );
});

test( 'dgghrd throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dgghrd( 'invalid', 2, 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dgghrd throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dgghrd( 'row-major', 2, 2, -1, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});
