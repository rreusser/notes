/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dtrsv = require( './../lib/dtrsv.js' );


// TESTS //

test( 'dtrsv is a function', function t() {
	assert.strictEqual( typeof dtrsv, 'function', 'is a function' );
});

test( 'dtrsv has expected arity', function t() {
	assert.strictEqual( dtrsv.length, 9, 'has expected arity' );
});

test( 'dtrsv throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dtrsv( 'invalid', 'upper', 'no-transpose', 'non-unit', new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 1 );
	}, TypeError );
});

test( 'dtrsv throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dtrsv( 'row-major', 'invalid', 'no-transpose', 'non-unit', new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 1 );
	}, TypeError );
});

test( 'dtrsv throws TypeError for invalid trans', function t() {
	assert.throws( function throws() {
		dtrsv( 'row-major', 'upper', 'invalid', 'non-unit', new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 1 );
	}, TypeError );
});

test( 'dtrsv throws TypeError for invalid diag', function t() {
	assert.throws( function throws() {
		dtrsv( 'row-major', 'upper', 'no-transpose', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 1 );
	}, TypeError );
});

test( 'dtrsv throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dtrsv( 'row-major', 'upper', 'no-transpose', 'non-unit', -1, new Float64Array( 4 ), 2, 2, 1 );
	}, RangeError );
});
