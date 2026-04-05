/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var ztrrfs = require( './../lib/ztrrfs.js' );


// TESTS //

test( 'ztrrfs is a function', function t() {
	assert.strictEqual( typeof ztrrfs, 'function', 'is a function' );
});

test( 'ztrrfs has expected arity', function t() {
	assert.strictEqual( ztrrfs.length, 19, 'has expected arity' );
});

test( 'ztrrfs throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		ztrrfs( 'invalid', 'no-transpose', 'non-unit', new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'ztrrfs throws TypeError for invalid trans', function t() {
	assert.throws( function throws() {
		ztrrfs( 'upper', 'invalid', 'non-unit', new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'ztrrfs throws TypeError for invalid diag', function t() {
	assert.throws( function throws() {
		ztrrfs( 'upper', 'no-transpose', 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'ztrrfs throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		ztrrfs( 'upper', 'no-transpose', 'non-unit', -1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'ztrrfs throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		ztrrfs( 'upper', 'no-transpose', 'non-unit', new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
