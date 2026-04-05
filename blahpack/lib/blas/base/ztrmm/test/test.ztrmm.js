/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var ztrmm = require( './../lib/ztrmm.js' );


// TESTS //

test( 'ztrmm is a function', function t() {
	assert.strictEqual( typeof ztrmm, 'function', 'is a function' );
});

test( 'ztrmm has expected arity', function t() {
	assert.strictEqual( ztrmm.length, 12, 'has expected arity' );
});

test( 'ztrmm throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		ztrmm( 'invalid', 'left', 'upper', 'no-transpose', 'non-unit', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'ztrmm throws TypeError for invalid side', function t() {
	assert.throws( function throws() {
		ztrmm( 'row-major', 'invalid', 'upper', 'no-transpose', 'non-unit', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'ztrmm throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		ztrmm( 'row-major', 'left', 'invalid', 'no-transpose', 'non-unit', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'ztrmm throws TypeError for invalid transa', function t() {
	assert.throws( function throws() {
		ztrmm( 'row-major', 'left', 'upper', 'invalid', 'non-unit', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'ztrmm throws TypeError for invalid diag', function t() {
	assert.throws( function throws() {
		ztrmm( 'row-major', 'left', 'upper', 'no-transpose', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'ztrmm throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		ztrmm( 'row-major', 'left', 'upper', 'no-transpose', 'non-unit', -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'ztrmm throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		ztrmm( 'row-major', 'left', 'upper', 'no-transpose', 'non-unit', new Float64Array( 4 ), -1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});
