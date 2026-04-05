/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var ztftri = require( './../lib/ztftri.js' );


// TESTS //

test( 'ztftri is a function', function t() {
	assert.strictEqual( typeof ztftri, 'function', 'is a function' );
});

test( 'ztftri has expected arity', function t() {
	assert.strictEqual( ztftri.length, 5, 'has expected arity' );
});

test( 'ztftri throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		ztftri( 2, 'invalid', 'non-unit', new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'ztftri throws TypeError for invalid diag', function t() {
	assert.throws( function throws() {
		ztftri( 2, 'upper', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'ztftri throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		ztftri( 2, 'upper', 'non-unit', -1, new Float64Array( 4 ) );
	}, RangeError );
});
