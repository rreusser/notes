/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlacon = require( './../lib/dlacon.js' );


// TESTS //

test( 'dlacon is a function', function t() {
	assert.strictEqual( typeof dlacon, 'function', 'is a function' );
});

test( 'dlacon has expected arity', function t() {
	assert.strictEqual( dlacon.length, 9, 'has expected arity' );
});

test( 'dlacon throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlacon( -1, 2, 1, 2, 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), new Float64Array( 4 ) );
	}, RangeError );
});
