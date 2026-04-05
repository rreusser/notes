/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlange = require( './../lib/dlange.js' );


// TESTS //

test( 'dlange is a function', function t() {
	assert.strictEqual( typeof dlange, 'function', 'is a function' );
});

test( 'dlange has expected arity', function t() {
	assert.strictEqual( dlange.length, 8, 'has expected arity' );
});

test( 'dlange throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dlange( 'invalid', 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dlange throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dlange( 'row-major', 2, -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'dlange throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlange( 'row-major', 2, new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});
