/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dsptrs = require( './../lib/dsptrs.js' );


// TESTS //

test( 'dsptrs is a function', function t() {
	assert.strictEqual( typeof dsptrs, 'function', 'is a function' );
});

test( 'dsptrs has expected arity', function t() {
	assert.strictEqual( dsptrs.length, 7, 'has expected arity' );
});

test( 'dsptrs throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dsptrs( 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dsptrs throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dsptrs( 'upper', -1, 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'dsptrs throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		dsptrs( 'upper', new Float64Array( 4 ), -1, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, RangeError );
});
