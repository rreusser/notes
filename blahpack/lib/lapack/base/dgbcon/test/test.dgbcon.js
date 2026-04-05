/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dgbcon = require( './../lib/dgbcon.js' );


// TESTS //

test( 'dgbcon is a function', function t() {
	assert.strictEqual( typeof dgbcon, 'function', 'is a function' );
});

test( 'dgbcon has expected arity', function t() {
	assert.strictEqual( dgbcon.length, 14, 'has expected arity' );
});

test( 'dgbcon throws TypeError for invalid norm', function t() {
	assert.throws( function throws() {
		dgbcon( 'invalid', new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dgbcon throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dgbcon( 'one-norm', -1, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
