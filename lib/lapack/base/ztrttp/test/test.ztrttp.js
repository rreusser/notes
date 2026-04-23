/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var ztrttp = require( './../lib/ztrttp.js' );


// TESTS //

test( 'ztrttp is a function', function t() {
	assert.strictEqual( typeof ztrttp, 'function', 'is a function' );
});

test( 'ztrttp has expected arity', function t() {
	assert.strictEqual( ztrttp.length, 6, 'has expected arity' );
});

test( 'ztrttp throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		ztrttp( 'invalid', 'upper', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ) );
	}, TypeError );
});

test( 'ztrttp throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		ztrttp( 'row-major', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ) );
	}, TypeError );
});

test( 'ztrttp throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		ztrttp( 'row-major', 'upper', -1, new Float64Array( 4 ), 2, new Float64Array( 4 ) );
	}, RangeError );
});
