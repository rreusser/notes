/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dgetri = require( './../lib/dgetri.js' );


// TESTS //

test( 'dgetri is a function', function t() {
	assert.strictEqual( typeof dgetri, 'function', 'is a function' );
});

test( 'dgetri has expected arity', function t() {
	assert.strictEqual( dgetri.length, 9, 'has expected arity' );
});

test( 'dgetri throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dgetri( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, 2 );
	}, TypeError );
});

test( 'dgetri throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dgetri( 'row-major', -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, 2 );
	}, RangeError );
});
