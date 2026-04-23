/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dpotrs = require( './../lib/dpotrs.js' );


// TESTS //

test( 'dpotrs is a function', function t() {
	assert.strictEqual( typeof dpotrs, 'function', 'is a function' );
});

test( 'dpotrs has expected arity', function t() {
	assert.strictEqual( dpotrs.length, 8, 'has expected arity' );
});

test( 'dpotrs throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dpotrs( 'invalid', 'upper', new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dpotrs throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dpotrs( 'row-major', 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dpotrs throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dpotrs( 'row-major', 'upper', -1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'dpotrs throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		dpotrs( 'row-major', 'upper', new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});
