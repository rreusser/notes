/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dtrmm = require( './../lib/dtrmm.js' );


// TESTS //

test( 'dtrmm is a function', function t() {
	assert.strictEqual( typeof dtrmm, 'function', 'is a function' );
});

test( 'dtrmm has expected arity', function t() {
	assert.strictEqual( dtrmm.length, 12, 'has expected arity' );
});

test( 'dtrmm throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dtrmm( 'invalid', 'left', 'upper', 'no-transpose', 'non-unit', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dtrmm throws TypeError for invalid side', function t() {
	assert.throws( function throws() {
		dtrmm( 'row-major', 'invalid', 'upper', 'no-transpose', 'non-unit', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dtrmm throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dtrmm( 'row-major', 'left', 'invalid', 'no-transpose', 'non-unit', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dtrmm throws TypeError for invalid transa', function t() {
	assert.throws( function throws() {
		dtrmm( 'row-major', 'left', 'upper', 'invalid', 'non-unit', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dtrmm throws TypeError for invalid diag', function t() {
	assert.throws( function throws() {
		dtrmm( 'row-major', 'left', 'upper', 'no-transpose', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dtrmm throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dtrmm( 'row-major', 'left', 'upper', 'no-transpose', 'non-unit', -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'dtrmm throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dtrmm( 'row-major', 'left', 'upper', 'no-transpose', 'non-unit', new Float64Array( 4 ), -1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});
