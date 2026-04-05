/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zbdsqr = require( './../lib/zbdsqr.js' );


// TESTS //

test( 'zbdsqr is a function', function t() {
	assert.strictEqual( typeof zbdsqr, 'function', 'is a function' );
});

test( 'zbdsqr has expected arity', function t() {
	assert.strictEqual( zbdsqr.length, 18, 'has expected arity' );
});

test( 'zbdsqr throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zbdsqr( 'invalid', 'upper', new Float64Array( 4 ), 2, 2, 2, 2, 1, 2, 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zbdsqr throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zbdsqr( 'row-major', 'invalid', new Float64Array( 4 ), 2, 2, 2, 2, 1, 2, 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zbdsqr throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zbdsqr( 'row-major', 'upper', -1, 2, 2, 2, 2, 1, 2, 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});
