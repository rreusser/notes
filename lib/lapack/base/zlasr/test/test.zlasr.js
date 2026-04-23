/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlasr = require( './../lib/zlasr.js' );


// TESTS //

test( 'zlasr is a function', function t() {
	assert.strictEqual( typeof zlasr, 'function', 'is a function' );
});

test( 'zlasr has expected arity', function t() {
	assert.strictEqual( zlasr.length, 12, 'has expected arity' );
});

test( 'zlasr throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zlasr( 'invalid', 'left', 2, 2, new Float64Array( 4 ), new Float64Array( 4 ), 2, 1, 2, 1, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zlasr throws TypeError for invalid side', function t() {
	assert.throws( function throws() {
		zlasr( 'row-major', 'invalid', 2, 2, new Float64Array( 4 ), new Float64Array( 4 ), 2, 1, 2, 1, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zlasr throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		zlasr( 'row-major', 'left', 2, 2, -1, new Float64Array( 4 ), 2, 1, 2, 1, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'zlasr throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zlasr( 'row-major', 'left', 2, 2, new Float64Array( 4 ), -1, 2, 1, 2, 1, new Float64Array( 4 ), 2 );
	}, RangeError );
});
