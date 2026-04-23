/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dpotrf = require( './../lib/dpotrf.js' );


// TESTS //

test( 'dpotrf is a function', function t() {
	assert.strictEqual( typeof dpotrf, 'function', 'is a function' );
});

test( 'dpotrf has expected arity', function t() {
	assert.strictEqual( dpotrf.length, 5, 'has expected arity' );
});

test( 'dpotrf throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dpotrf( 'invalid', 'upper', new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dpotrf throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dpotrf( 'row-major', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dpotrf throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dpotrf( 'row-major', 'upper', -1, new Float64Array( 4 ), 2 );
	}, RangeError );
});
