/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dtrrfs = require( './../lib/dtrrfs.js' );


// TESTS //

test( 'dtrrfs is a function', function t() {
	assert.strictEqual( typeof dtrrfs, 'function', 'is a function' );
});

test( 'dtrrfs has expected arity', function t() {
	assert.strictEqual( dtrrfs.length, 19, 'has expected arity' );
});

test( 'dtrrfs throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dtrrfs( 'invalid', 'no-transpose', 'non-unit', new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dtrrfs throws TypeError for invalid trans', function t() {
	assert.throws( function throws() {
		dtrrfs( 'upper', 'invalid', 'non-unit', new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dtrrfs throws TypeError for invalid diag', function t() {
	assert.throws( function throws() {
		dtrrfs( 'upper', 'no-transpose', 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dtrrfs throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dtrrfs( 'upper', 'no-transpose', 'non-unit', -1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'dtrrfs throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		dtrrfs( 'upper', 'no-transpose', 'non-unit', new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
