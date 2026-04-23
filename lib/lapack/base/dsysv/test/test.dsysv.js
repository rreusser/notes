/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dsysv = require( './../lib/dsysv.js' );


// TESTS //

test( 'dsysv is a function', function t() {
	assert.strictEqual( typeof dsysv, 'function', 'is a function' );
});

test( 'dsysv has expected arity', function t() {
	assert.strictEqual( dsysv.length, 9, 'has expected arity' );
});

test( 'dsysv throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dsysv( 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dsysv throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dsysv( 'upper', -1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'dsysv throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		dsysv( 'upper', new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2 );
	}, RangeError );
});
