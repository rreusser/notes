/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dsytrf = require( './../lib/dsytrf.js' );


// TESTS //

test( 'dsytrf is a function', function t() {
	assert.strictEqual( typeof dsytrf, 'function', 'is a function' );
});

test( 'dsytrf has expected arity', function t() {
	assert.strictEqual( dsytrf.length, 7, 'has expected arity' );
});

test( 'dsytrf throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dsytrf( 'invalid', 'upper', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dsytrf throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dsytrf( 'row-major', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dsytrf throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dsytrf( 'row-major', 'upper', -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});
