/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dspgvx = require( './../lib/dspgvx.js' );


// TESTS //

test( 'dspgvx is a function', function t() {
	assert.strictEqual( typeof dspgvx, 'function', 'is a function' );
});

test( 'dspgvx has expected arity', function t() {
	assert.strictEqual( dspgvx.length, 20, 'has expected arity' );
});

test( 'dspgvx throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dspgvx( 'invalid', 2, 2, 2, 'upper', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 2, 2, 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'dspgvx throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dspgvx( 'row-major', 2, 2, 2, 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 2, 2, 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'dspgvx throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dspgvx( 'row-major', 2, 2, 2, 'upper', -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 2, 2, 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ) );
	}, RangeError );
});
