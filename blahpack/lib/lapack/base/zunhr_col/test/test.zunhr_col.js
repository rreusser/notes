

/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zunhr_col = require( './../lib/zunhr_col.js' );


// TESTS //

test( 'zunhr_col is a function', function t() {
	assert.strictEqual( typeof zunhr_col, 'function', 'is a function' );
});

test( 'zunhr_col has expected arity', function t() {
	assert.strictEqual( zunhr_col.length, 10, 'has expected arity' );
});

test( 'zunhr_col throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zunhr_col( 'invalid', 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zunhr_col throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		zunhr_col( 'row-major', -1, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'zunhr_col throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zunhr_col( 'row-major', 2, -1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});

