
/* eslint-disable no-restricted-syntax, stdlib/first-unit-test, camelcase */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dla_porcond = require( './../lib/dla_porcond.js' );


// TESTS //

test( 'dla_porcond is a function', function t() {
	assert.strictEqual( typeof dla_porcond, 'function', 'is a function' );
});

test( 'dla_porcond has expected arity', function t() {
	assert.strictEqual( dla_porcond.length, 15, 'has expected arity' );
});

test( 'dla_porcond throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dla_porcond( 'invalid', 'upper', 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 2 );
	}, TypeError );
});

test( 'dla_porcond throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dla_porcond( 'row-major', 'invalid', 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 2 );
	}, TypeError );
});

test( 'dla_porcond throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dla_porcond( 'row-major', 'upper', -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 2 );
	}, RangeError );
});
