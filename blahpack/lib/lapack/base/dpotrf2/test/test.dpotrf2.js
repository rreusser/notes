/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dpotrf2 = require( './../lib/dpotrf2.js' );


// TESTS //

test( 'dpotrf2 is a function', function t() {
	assert.strictEqual( typeof dpotrf2, 'function', 'is a function' );
});

test( 'dpotrf2 has expected arity', function t() {
	assert.strictEqual( dpotrf2.length, 5, 'has expected arity' );
});

test( 'dpotrf2 throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dpotrf2( 'invalid', 'upper', new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dpotrf2 throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dpotrf2( 'row-major', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dpotrf2 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dpotrf2( 'row-major', 'upper', -1, new Float64Array( 4 ), 2 );
	}, RangeError );
});
