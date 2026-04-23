/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var ztprfs = require( './../lib/ztprfs.js' );


// TESTS //

test( 'ztprfs is a function', function t() {
	assert.strictEqual( typeof ztprfs, 'function', 'is a function' );
});

test( 'ztprfs has expected arity', function t() {
	assert.strictEqual( ztprfs.length, 18, 'has expected arity' );
});

test( 'ztprfs throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		ztprfs( 'invalid', 'no-transpose', 'non-unit', new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'ztprfs throws TypeError for invalid trans', function t() {
	assert.throws( function throws() {
		ztprfs( 'upper', 'invalid', 'non-unit', new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'ztprfs throws TypeError for invalid diag', function t() {
	assert.throws( function throws() {
		ztprfs( 'upper', 'no-transpose', 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'ztprfs throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		ztprfs( 'upper', 'no-transpose', 'non-unit', -1, 2, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'ztprfs throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		ztprfs( 'upper', 'no-transpose', 'non-unit', new Float64Array( 4 ), -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
