/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dtrsen = require( './../lib/dtrsen.js' );


// TESTS //

test( 'dtrsen is a function', function t() {
	assert.strictEqual( typeof dtrsen, 'function', 'is a function' );
});

test( 'dtrsen has expected arity', function t() {
	assert.strictEqual( dtrsen.length, 22, 'has expected arity' );
});

test( 'dtrsen throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dtrsen( 2, 2, new Float64Array( 4 ), 1, -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 1, 2, new Float64Array( 4 ), 1, 2 );
	}, RangeError );
});

test( 'dtrsen throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dtrsen( 2, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, -1, 2, 2, new Float64Array( 4 ), 1, 2, new Float64Array( 4 ), 1, 2 );
	}, RangeError );
});
