/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlauum = require( './../lib/dlauum.js' );


// TESTS //

test( 'dlauum is a function', function t() {
	assert.strictEqual( typeof dlauum, 'function', 'is a function' );
});

test( 'dlauum has expected arity', function t() {
	assert.strictEqual( dlauum.length, 5, 'has expected arity' );
});

test( 'dlauum throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dlauum( 'invalid', 'upper', new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dlauum throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dlauum( 'row-major', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dlauum throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlauum( 'row-major', 'upper', -1, new Float64Array( 4 ), 2 );
	}, RangeError );
});
