/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dgehd2 = require( './../lib/dgehd2.js' );


// TESTS //

test( 'dgehd2 is a function', function t() {
	assert.strictEqual( typeof dgehd2, 'function', 'is a function' );
});

test( 'dgehd2 has expected arity', function t() {
	assert.strictEqual( dgehd2.length, 10, 'has expected arity' );
});

test( 'dgehd2 throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dgehd2( 'invalid', new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dgehd2 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dgehd2( 'row-major', -1, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
