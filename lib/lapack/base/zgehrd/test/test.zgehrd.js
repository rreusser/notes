/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zgehrd = require( './../lib/zgehrd.js' );


// TESTS //

test( 'zgehrd is a function', function t() {
	assert.strictEqual( typeof zgehrd, 'function', 'is a function' );
});

test( 'zgehrd has expected arity', function t() {
	assert.strictEqual( zgehrd.length, 13, 'has expected arity' );
});

test( 'zgehrd throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zgehrd( -1, 2, 2, new Float64Array( 4 ), 1, 1, 2, new Float64Array( 4 ), 1, 2, new Float64Array( 4 ), 1, 2 );
	}, RangeError );
});
