
/* eslint-disable no-restricted-syntax, stdlib/first-unit-test, camelcase, max-len */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var Complex128Array = require( '@stdlib/array/complex128' );
var zla_gbrcond_x = require( './../lib' );


// TESTS //

test( 'main export is a function', function t() {
	assert.strictEqual( typeof zla_gbrcond_x, 'function', 'main export is a function' ); // eslint-disable-line max-len
});

test( 'main export has an ndarray method', function t() {
	assert.strictEqual( typeof zla_gbrcond_x.ndarray, 'function', 'has ndarray method' ); // eslint-disable-line max-len
});

test( 'ndarray returns 1.0 when N=0', function t() {
	var result;
	var RWORK;
	var WORK;
	var IPIV;
	var AFB;
	var AB;
	var x;

	RWORK = new Float64Array( 1 );
	WORK = new Complex128Array( 2 );
	IPIV = new Int32Array( 1 );
	AFB = new Complex128Array( 1 );
	AB = new Complex128Array( 1 );
	x = new Complex128Array( 1 );
	result = zla_gbrcond_x.ndarray( 'no-transpose', 0, 0, 0, AB, 1, 1, 0, AFB, 1, 1, 0, IPIV, 1, 0, x, 1, 0, WORK, 1, 0, RWORK, 1, 0 ); // eslint-disable-line max-len
	assert.strictEqual( result, 1.0, 'returns 1.0 for N=0' );
});
