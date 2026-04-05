/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var ztrsm = require( './../lib/ztrsm.js' );


// TESTS //

test( 'ztrsm is a function', function t() {
	assert.strictEqual( typeof ztrsm, 'function', 'is a function' );
});

test( 'ztrsm has expected arity', function t() {
	assert.strictEqual( ztrsm.length, 12, 'has expected arity' );
});

test( 'ztrsm throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		ztrsm( 'invalid', 'left', 'upper', 'no-transpose', 'non-unit', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'ztrsm throws TypeError for invalid side', function t() {
	assert.throws( function throws() {
		ztrsm( 'row-major', 'invalid', 'upper', 'no-transpose', 'non-unit', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'ztrsm throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		ztrsm( 'row-major', 'left', 'invalid', 'no-transpose', 'non-unit', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'ztrsm throws TypeError for invalid transa', function t() {
	assert.throws( function throws() {
		ztrsm( 'row-major', 'left', 'upper', 'invalid', 'non-unit', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'ztrsm throws TypeError for invalid diag', function t() {
	assert.throws( function throws() {
		ztrsm( 'row-major', 'left', 'upper', 'no-transpose', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'ztrsm throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		ztrsm( 'row-major', 'left', 'upper', 'no-transpose', 'non-unit', -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'ztrsm throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		ztrsm( 'row-major', 'left', 'upper', 'no-transpose', 'non-unit', new Float64Array( 4 ), -1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});
