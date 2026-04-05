/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dtgsja = require( './../lib/dtgsja.js' );


// TESTS //

test( 'dtgsja is a function', function t() {
	assert.strictEqual( typeof dtgsja, 'function', 'is a function' );
});

test( 'dtgsja has expected arity', function t() {
	assert.strictEqual( dtgsja.length, 27, 'has expected arity' );
});

test( 'dtgsja throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dtgsja( 2, 2, 2, -1, 2, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2 );
	}, RangeError );
});

test( 'dtgsja throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dtgsja( 2, 2, 2, new Float64Array( 4 ), 2, -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2 );
	}, RangeError );
});

test( 'dtgsja throws RangeError for negative K', function t() {
	assert.throws( function throws() {
		dtgsja( 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), -1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2 );
	}, RangeError );
});
