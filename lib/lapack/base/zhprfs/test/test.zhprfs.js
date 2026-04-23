/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zhprfs = require( './../lib/zhprfs.js' );


// TESTS //

test( 'zhprfs is a function', function t() {
	assert.strictEqual( typeof zhprfs, 'function', 'is a function' );
});

test( 'zhprfs has expected arity', function t() {
	assert.strictEqual( zhprfs.length, 13, 'has expected arity' );
});

test( 'zhprfs throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zhprfs( 'invalid', 'upper', new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'zhprfs throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zhprfs( 'row-major', 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'zhprfs throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zhprfs( 'row-major', 'upper', -1, 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ) );
	}, RangeError );
});

test( 'zhprfs throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		zhprfs( 'row-major', 'upper', new Float64Array( 4 ), -1, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ) );
	}, RangeError );
});
