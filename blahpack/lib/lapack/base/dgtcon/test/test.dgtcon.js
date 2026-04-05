/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dgtcon = require( './../lib/dgtcon.js' );


// TESTS //

test( 'dgtcon is a function', function t() {
	assert.strictEqual( typeof dgtcon, 'function', 'is a function' );
});

test( 'dgtcon has expected arity', function t() {
	assert.strictEqual( dgtcon.length, 18, 'has expected arity' );
});

test( 'dgtcon throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dgtcon( 2, -1, new Float64Array( 4 ), 1, 2, 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, 2, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
