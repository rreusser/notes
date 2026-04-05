/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dtprfs = require( './../lib/dtprfs.js' );


// TESTS //

test( 'dtprfs is a function', function t() {
	assert.strictEqual( typeof dtprfs, 'function', 'is a function' );
});

test( 'dtprfs has expected arity', function t() {
	assert.strictEqual( dtprfs.length, 18, 'has expected arity' );
});

test( 'dtprfs throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dtprfs( 'invalid', 'no-transpose', 'non-unit', new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dtprfs throws TypeError for invalid trans', function t() {
	assert.throws( function throws() {
		dtprfs( 'upper', 'invalid', 'non-unit', new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dtprfs throws TypeError for invalid diag', function t() {
	assert.throws( function throws() {
		dtprfs( 'upper', 'no-transpose', 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dtprfs throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dtprfs( 'upper', 'no-transpose', 'non-unit', -1, 2, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'dtprfs throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		dtprfs( 'upper', 'no-transpose', 'non-unit', new Float64Array( 4 ), -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
