/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dsyconRook = require( './../lib/dsycon_rook.js' );


// TESTS //

test( 'dsyconRook is a function', function t() {
	assert.strictEqual( typeof dsyconRook, 'function', 'is a function' );
});

test( 'dsyconRook has expected arity', function t() {
	assert.strictEqual( dsyconRook.length, 15, 'has expected arity' );
});

test( 'dsyconRook throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dsyconRook( 'invalid', 'upper', 2, new Float64Array( 4 ), 2, 2, 2, 2, 2, 2, new Float64Array( 4 ), 2, 2, 2, 2 );
	}, TypeError );
});

test( 'dsyconRook throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dsyconRook( 'row-major', 'invalid', 2, new Float64Array( 4 ), 2, 2, 2, 2, 2, 2, new Float64Array( 4 ), 2, 2, 2, 2 );
	}, TypeError );
});

test( 'dsyconRook throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dsyconRook( 'row-major', 'upper', -1, new Float64Array( 4 ), 2, 2, 2, 2, 2, 2, new Float64Array( 4 ), 2, 2, 2, 2 );
	}, RangeError );
});

test( 'dsyconRook throws RangeError for LDA < max(1,N)', function t() {
	assert.throws( function throws() {
		dsyconRook( 'row-major', 'upper', 3, new Float64Array( 9 ), 1, new Int32Array( 3 ), 1, 0, 1.0, new Float64Array( 1 ), new Float64Array( 6 ), 1, new Int32Array( 3 ), 1, 0 );
	}, RangeError );
});

test( 'dsyconRook computes rcond=1 for N=0 (column-major)', function t() {
	var rcond;
	var info;
	rcond = new Float64Array( 1 );
	info = dsyconRook( 'column-major', 'upper', 0, new Float64Array( 1 ), 1, new Int32Array( 1 ), 1, 0, 0.0, rcond, new Float64Array( 2 ), 1, new Int32Array( 1 ), 1, 0 ); // eslint-disable-line max-len
	assert.strictEqual( info, 0 );
	assert.strictEqual( rcond[ 0 ], 1.0 );
});

test( 'dsyconRook computes rcond=1 for N=0 (row-major)', function t() {
	var rcond;
	var info;
	rcond = new Float64Array( 1 );
	info = dsyconRook( 'row-major', 'upper', 0, new Float64Array( 1 ), 1, new Int32Array( 1 ), 1, 0, 0.0, rcond, new Float64Array( 2 ), 1, new Int32Array( 1 ), 1, 0 ); // eslint-disable-line max-len
	assert.strictEqual( info, 0 );
	assert.strictEqual( rcond[ 0 ], 1.0 );
});
