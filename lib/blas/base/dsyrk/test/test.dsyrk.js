/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dsyrk = require( './../lib/dsyrk.js' );


// TESTS //

test( 'dsyrk is a function', function t() {
	assert.strictEqual( typeof dsyrk, 'function', 'is a function' );
});

test( 'dsyrk has expected arity', function t() {
	assert.strictEqual( dsyrk.length, 11, 'has expected arity' );
});

test( 'dsyrk throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dsyrk( 'invalid', 'upper', 'no-transpose', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dsyrk throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dsyrk( 'row-major', 'invalid', 'no-transpose', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dsyrk throws TypeError for invalid trans', function t() {
	assert.throws( function throws() {
		dsyrk( 'row-major', 'upper', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dsyrk throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dsyrk( 'row-major', 'upper', 'no-transpose', -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'dsyrk throws RangeError for negative K', function t() {
	assert.throws( function throws() {
		dsyrk( 'row-major', 'upper', 'no-transpose', new Float64Array( 4 ), -1, 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});
