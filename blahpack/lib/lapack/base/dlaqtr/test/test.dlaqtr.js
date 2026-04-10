

/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlaqtr = require( './../lib/dlaqtr.js' );


// TESTS //

test( 'dlaqtr is a function', function t() {
	assert.strictEqual( typeof dlaqtr, 'function', 'is a function' );
});

test( 'dlaqtr has expected arity', function t() {
	assert.strictEqual( dlaqtr.length, 14, 'has expected arity' );
});

test( 'dlaqtr throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dlaqtr( 'invalid', 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dlaqtr throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlaqtr( 'row-major', 2, 2, -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});

