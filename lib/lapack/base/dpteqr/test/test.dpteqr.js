/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dpteqr = require( './../lib/dpteqr.js' );


// TESTS //

test( 'dpteqr is a function', function t() {
	assert.strictEqual( typeof dpteqr, 'function', 'is a function' );
});

test( 'dpteqr has expected arity', function t() {
	assert.strictEqual( dpteqr.length, 11, 'has expected arity' );
});

test( 'dpteqr throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dpteqr( 'invalid', 2, new Float64Array( 4 ), 2, 1, 2, 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dpteqr throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dpteqr( 'row-major', 2, -1, 2, 1, 2, 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});
