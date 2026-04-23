/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dgees = require( './../lib/dgees.js' );


// TESTS //

test( 'dgees is a function', function t() {
	assert.strictEqual( typeof dgees, 'function', 'is a function' );
});

test( 'dgees has expected arity', function t() {
	assert.strictEqual( dgees.length, 18, 'has expected arity' );
});

test( 'dgees throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dgees( 2, 2, 2, -1, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});
