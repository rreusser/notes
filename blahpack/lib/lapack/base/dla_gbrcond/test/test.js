
/* eslint-disable no-restricted-syntax, stdlib/first-unit-test, camelcase, max-len */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dla_gbrcond = require( './../lib' );


// TESTS //

test( 'main export is a function', function t() {
	assert.strictEqual( typeof dla_gbrcond, 'function', 'main export is a function' ); // eslint-disable-line max-len
});

test( 'main export has an ndarray method', function t() {
	assert.strictEqual( typeof dla_gbrcond.ndarray, 'function', 'has ndarray method' ); // eslint-disable-line max-len
});

test( 'ndarray returns 1.0 when N=0', function t() {
	var result;
	var IWORK;
	var WORK;
	var IPIV;
	var AFB;
	var AB;
	var c;

	IWORK = new Int32Array( 1 );
	WORK = new Float64Array( 10 );
	IPIV = new Int32Array( 1 );
	AFB = new Float64Array( 1 );
	AB = new Float64Array( 1 );
	c = new Float64Array( 1 );
	result = dla_gbrcond.ndarray( 'no-transpose', 0, 0, 0, AB, 1, 1, 0, AFB, 1, 1, 0, IPIV, 1, 0, 1, c, 1, 0, WORK, 1, 0, IWORK, 1, 0 ); // eslint-disable-line max-len
	assert.strictEqual( result, 1.0, 'returns 1.0 for N=0' );
});
