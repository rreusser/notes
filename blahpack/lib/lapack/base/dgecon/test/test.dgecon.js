/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dgecon = require( './../lib/dgecon.js' );


// TESTS //

test( 'dgecon is a function', function t() {
	assert.strictEqual( typeof dgecon, 'function', 'is a function' );
});

test( 'dgecon has expected arity', function t() {
	assert.strictEqual( dgecon.length, 11, 'has expected arity' );
});

test( 'dgecon throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dgecon( 'invalid', 'one-norm', new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dgecon throws TypeError for invalid norm', function t() {
	assert.throws( function throws() {
		dgecon( 'row-major', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dgecon throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dgecon( 'row-major', 'one-norm', -1, new Float64Array( 4 ), 2, 2, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
