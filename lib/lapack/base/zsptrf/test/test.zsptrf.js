/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zsptrf = require( './../lib/zsptrf.js' );


// TESTS //

test( 'zsptrf is a function', function t() {
	assert.strictEqual( typeof zsptrf, 'function', 'is a function' );
});

test( 'zsptrf has expected arity', function t() {
	assert.strictEqual( zsptrf.length, 4, 'has expected arity' );
});

test( 'zsptrf throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zsptrf( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'zsptrf throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zsptrf( 'upper', -1, new Float64Array( 4 ), new Float64Array( 4 ) );
	}, RangeError );
});
