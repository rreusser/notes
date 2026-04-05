/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dspev = require( './../lib/dspev.js' );


// TESTS //

test( 'dspev is a function', function t() {
	assert.strictEqual( typeof dspev, 'function', 'is a function' );
});

test( 'dspev has expected arity', function t() {
	assert.strictEqual( dspev.length, 9, 'has expected arity' );
});

test( 'dspev throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dspev( 'invalid', 2, 'upper', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ) );
	}, TypeError );
});

test( 'dspev throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dspev( 'row-major', 2, 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ) );
	}, TypeError );
});

test( 'dspev throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dspev( 'row-major', 2, 'upper', -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ) );
	}, RangeError );
});
