/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dsptrf = require( './../lib/dsptrf.js' );


// TESTS //

test( 'dsptrf is a function', function t() {
	assert.strictEqual( typeof dsptrf, 'function', 'is a function' );
});

test( 'dsptrf has expected arity', function t() {
	assert.strictEqual( dsptrf.length, 4, 'has expected arity' );
});

test( 'dsptrf throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dsptrf( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'dsptrf throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dsptrf( 'upper', -1, new Float64Array( 4 ), new Float64Array( 4 ) );
	}, RangeError );
});
