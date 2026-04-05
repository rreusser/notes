/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var ztptri = require( './../lib/ztptri.js' );


// TESTS //

test( 'ztptri is a function', function t() {
	assert.strictEqual( typeof ztptri, 'function', 'is a function' );
});

test( 'ztptri has expected arity', function t() {
	assert.strictEqual( ztptri.length, 4, 'has expected arity' );
});

test( 'ztptri throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		ztptri( 'invalid', 'non-unit', new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'ztptri throws TypeError for invalid diag', function t() {
	assert.throws( function throws() {
		ztptri( 'upper', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'ztptri throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		ztptri( 'upper', 'non-unit', -1, new Float64Array( 4 ) );
	}, RangeError );
});
