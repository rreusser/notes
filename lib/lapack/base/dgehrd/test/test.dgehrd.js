/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dgehrd = require( './../lib/dgehrd.js' );


// TESTS //

test( 'dgehrd is a function', function t() {
	assert.strictEqual( typeof dgehrd, 'function', 'is a function' );
});

test( 'dgehrd has expected arity', function t() {
	assert.strictEqual( dgehrd.length, 10, 'has expected arity' );
});

test( 'dgehrd throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dgehrd( 'invalid', new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dgehrd throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dgehrd( 'row-major', -1, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
