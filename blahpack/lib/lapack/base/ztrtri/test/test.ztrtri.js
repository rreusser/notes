/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var ztrtri = require( './../lib/ztrtri.js' );


// TESTS //

test( 'ztrtri is a function', function t() {
	assert.strictEqual( typeof ztrtri, 'function', 'is a function' );
});

test( 'ztrtri has expected arity', function t() {
	assert.strictEqual( ztrtri.length, 6, 'has expected arity' );
});

test( 'ztrtri throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		ztrtri( 'invalid', 'upper', 'non-unit', new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'ztrtri throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		ztrtri( 'row-major', 'invalid', 'non-unit', new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'ztrtri throws TypeError for invalid diag', function t() {
	assert.throws( function throws() {
		ztrtri( 'row-major', 'upper', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'ztrtri throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		ztrtri( 'row-major', 'upper', 'non-unit', -1, new Float64Array( 4 ), 2 );
	}, RangeError );
});
