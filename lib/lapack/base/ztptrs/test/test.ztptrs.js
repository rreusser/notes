/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var ztptrs = require( './../lib/ztptrs.js' );


// TESTS //

test( 'ztptrs is a function', function t() {
	assert.strictEqual( typeof ztptrs, 'function', 'is a function' );
});

test( 'ztptrs has expected arity', function t() {
	assert.strictEqual( ztptrs.length, 9, 'has expected arity' );
});

test( 'ztptrs throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		ztptrs( 'invalid', 'upper', 'no-transpose', 'non-unit', new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'ztptrs throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		ztptrs( 'row-major', 'invalid', 'no-transpose', 'non-unit', new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'ztptrs throws TypeError for invalid trans', function t() {
	assert.throws( function throws() {
		ztptrs( 'row-major', 'upper', 'invalid', 'non-unit', new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'ztptrs throws TypeError for invalid diag', function t() {
	assert.throws( function throws() {
		ztptrs( 'row-major', 'upper', 'no-transpose', 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'ztptrs throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		ztptrs( 'row-major', 'upper', 'no-transpose', 'non-unit', -1, 2, new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'ztptrs throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		ztptrs( 'row-major', 'upper', 'no-transpose', 'non-unit', new Float64Array( 4 ), -1, new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, RangeError );
});
