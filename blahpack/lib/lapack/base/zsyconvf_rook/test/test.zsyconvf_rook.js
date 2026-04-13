/* eslint-disable no-restricted-syntax, stdlib/first-unit-test, camelcase */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );
var zsyconvf_rook = require( './../lib/zsyconvf_rook.js' );


// TESTS //

test( 'zsyconvf_rook is a function', function t() {
	assert.strictEqual( typeof zsyconvf_rook, 'function', 'is a function' );
});

test( 'zsyconvf_rook has expected arity', function t() {
	assert.strictEqual( zsyconvf_rook.length, 11, 'has expected arity' );
});

test( 'zsyconvf_rook throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zsyconvf_rook( 'invalid', 'upper', 'convert', 2, new Complex128Array( 4 ), 2, new Complex128Array( 2 ), 1, new Int32Array( 2 ), 1, 0 );
	}, TypeError );
});

test( 'zsyconvf_rook throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zsyconvf_rook( 'row-major', 'invalid', 'convert', 2, new Complex128Array( 4 ), 2, new Complex128Array( 2 ), 1, new Int32Array( 2 ), 1, 0 );
	}, TypeError );
});

test( 'zsyconvf_rook throws TypeError for invalid way', function t() {
	assert.throws( function throws() {
		zsyconvf_rook( 'row-major', 'upper', 'invalid', 2, new Complex128Array( 4 ), 2, new Complex128Array( 2 ), 1, new Int32Array( 2 ), 1, 0 );
	}, TypeError );
});

test( 'zsyconvf_rook throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zsyconvf_rook( 'row-major', 'upper', 'convert', -1, new Complex128Array( 4 ), 2, new Complex128Array( 2 ), 1, new Int32Array( 2 ), 1, 0 );
	}, RangeError );
});

test( 'zsyconvf_rook throws RangeError for LDA < max(1,N) in row-major', function t() {
	assert.throws( function throws() {
		zsyconvf_rook( 'row-major', 'upper', 'convert', 4, new Complex128Array( 16 ), 2, new Complex128Array( 4 ), 1, new Int32Array( 4 ), 1, 0 );
	}, RangeError );
});

test( 'zsyconvf_rook returns 0 for N=0 (row-major)', function t() {
	var info = zsyconvf_rook( 'row-major', 'upper', 'convert', 0, new Complex128Array( 0 ), 1, new Complex128Array( 0 ), 1, new Int32Array( 0 ), 1, 0 );
	assert.strictEqual( info, 0, 'returns 0' );
});

test( 'zsyconvf_rook returns 0 for N=0 (column-major)', function t() {
	var info = zsyconvf_rook( 'column-major', 'lower', 'revert', 0, new Complex128Array( 0 ), 1, new Complex128Array( 0 ), 1, new Int32Array( 0 ), 1, 0 );
	assert.strictEqual( info, 0, 'returns 0' );
});
