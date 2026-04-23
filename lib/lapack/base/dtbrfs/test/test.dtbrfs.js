/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dtbrfs = require( './../lib/dtbrfs.js' );


// TESTS //

test( 'dtbrfs is a function', function t() {
	assert.strictEqual( typeof dtbrfs, 'function', 'is a function' );
});

test( 'dtbrfs has expected arity', function t() {
	assert.strictEqual( dtbrfs.length, 20, 'has expected arity' );
});

test( 'dtbrfs throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dtbrfs( 'invalid', 'no-transpose', 'non-unit', new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dtbrfs throws TypeError for invalid trans', function t() {
	assert.throws( function throws() {
		dtbrfs( 'upper', 'invalid', 'non-unit', new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dtbrfs throws TypeError for invalid diag', function t() {
	assert.throws( function throws() {
		dtbrfs( 'upper', 'no-transpose', 'invalid', new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dtbrfs throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dtbrfs( 'upper', 'no-transpose', 'non-unit', -1, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'dtbrfs throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		dtbrfs( 'upper', 'no-transpose', 'non-unit', new Float64Array( 4 ), 2, -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
