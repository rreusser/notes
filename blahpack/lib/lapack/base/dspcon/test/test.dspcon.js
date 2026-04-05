/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dspcon = require( './../lib/dspcon.js' );


// TESTS //

test( 'dspcon is a function', function t() {
	assert.strictEqual( typeof dspcon, 'function', 'is a function' );
});

test( 'dspcon has expected arity', function t() {
	assert.strictEqual( dspcon.length, 8, 'has expected arity' );
});

test( 'dspcon throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dspcon( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'dspcon throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dspcon( 'upper', -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), new Float64Array( 4 ) );
	}, RangeError );
});
