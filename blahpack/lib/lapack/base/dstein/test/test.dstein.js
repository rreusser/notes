/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dstein = require( './../lib/dstein.js' );


// TESTS //

test( 'dstein is a function', function t() {
	assert.strictEqual( typeof dstein, 'function', 'is a function' );
});

test( 'dstein has expected arity', function t() {
	assert.strictEqual( dstein.length, 20, 'has expected arity' );
});

test( 'dstein throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dstein( -1, 2, 1, 2, 1, new Float64Array( 4 ), 2, 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'dstein throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dstein( new Float64Array( 4 ), 2, 1, 2, 1, -1, 2, 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
