/* eslint-disable no-restricted-syntax, stdlib/first-unit-test, camelcase */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dorhr_col = require( './../lib/dorhr_col.js' );


// TESTS //

test( 'dorhr_col is a function', function t() {
	assert.strictEqual( typeof dorhr_col, 'function', 'is a function' );
});

test( 'dorhr_col has expected arity', function t() {
	assert.strictEqual( dorhr_col.length, 10, 'has expected arity' );
});

test( 'dorhr_col throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dorhr_col( 'invalid', 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dorhr_col throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dorhr_col( 'row-major', -1, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'dorhr_col throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dorhr_col( 'row-major', 2, -1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});
