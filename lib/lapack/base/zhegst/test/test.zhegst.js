/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zhegst = require( './../lib/zhegst.js' );


// TESTS //

test( 'zhegst is a function', function t() {
	assert.strictEqual( typeof zhegst, 'function', 'is a function' );
});

test( 'zhegst has expected arity', function t() {
	assert.strictEqual( zhegst.length, 7, 'has expected arity' );
});

test( 'zhegst throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zhegst( 2, 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zhegst throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zhegst( 2, 'upper', -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});
