/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zhfrk = require( './../lib/zhfrk.js' );


// TESTS //

test( 'zhfrk is a function', function t() {
	assert.strictEqual( typeof zhfrk, 'function', 'is a function' );
});

test( 'zhfrk has expected arity', function t() {
	assert.strictEqual( zhfrk.length, 10, 'has expected arity' );
});

test( 'zhfrk throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zhfrk( 2, 'invalid', 'no-transpose', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ) );
	}, TypeError );
});

test( 'zhfrk throws TypeError for invalid trans', function t() {
	assert.throws( function throws() {
		zhfrk( 2, 'upper', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ) );
	}, TypeError );
});

test( 'zhfrk throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zhfrk( 2, 'upper', 'no-transpose', -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ) );
	}, RangeError );
});

test( 'zhfrk throws RangeError for negative K', function t() {
	assert.throws( function throws() {
		zhfrk( 2, 'upper', 'no-transpose', new Float64Array( 4 ), -1, 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ) );
	}, RangeError );
});
