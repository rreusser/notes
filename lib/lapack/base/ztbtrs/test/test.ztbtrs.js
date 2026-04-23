/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var ztbtrs = require( './../lib/ztbtrs.js' );


// TESTS //

test( 'ztbtrs is a function', function t() {
	assert.strictEqual( typeof ztbtrs, 'function', 'is a function' );
});

test( 'ztbtrs has expected arity', function t() {
	assert.strictEqual( ztbtrs.length, 11, 'has expected arity' );
});

test( 'ztbtrs throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		ztbtrs( 'invalid', 'upper', 'no-transpose', 'non-unit', new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'ztbtrs throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		ztbtrs( 'row-major', 'invalid', 'no-transpose', 'non-unit', new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'ztbtrs throws TypeError for invalid trans', function t() {
	assert.throws( function throws() {
		ztbtrs( 'row-major', 'upper', 'invalid', 'non-unit', new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'ztbtrs throws TypeError for invalid diag', function t() {
	assert.throws( function throws() {
		ztbtrs( 'row-major', 'upper', 'no-transpose', 'invalid', new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'ztbtrs throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		ztbtrs( 'row-major', 'upper', 'no-transpose', 'non-unit', -1, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'ztbtrs throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		ztbtrs( 'row-major', 'upper', 'no-transpose', 'non-unit', new Float64Array( 4 ), 2, -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});
