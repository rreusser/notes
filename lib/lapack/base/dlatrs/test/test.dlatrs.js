/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlatrs = require( './../lib/dlatrs.js' );


// TESTS //

test( 'dlatrs is a function', function t() {
	assert.strictEqual( typeof dlatrs, 'function', 'is a function' );
});

test( 'dlatrs has expected arity', function t() {
	assert.strictEqual( dlatrs.length, 13, 'has expected arity' );
});

test( 'dlatrs throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dlatrs( 'invalid', 'upper', 'no-transpose', 'non-unit', 2, new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 1, 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dlatrs throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dlatrs( 'row-major', 'invalid', 'no-transpose', 'non-unit', 2, new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 1, 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dlatrs throws TypeError for invalid trans', function t() {
	assert.throws( function throws() {
		dlatrs( 'row-major', 'upper', 'invalid', 'non-unit', 2, new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 1, 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dlatrs throws TypeError for invalid diag', function t() {
	assert.throws( function throws() {
		dlatrs( 'row-major', 'upper', 'no-transpose', 'invalid', 2, new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 1, 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dlatrs throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlatrs( 'row-major', 'upper', 'no-transpose', 'non-unit', 2, -1, new Float64Array( 4 ), 2, 2, 1, 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});
