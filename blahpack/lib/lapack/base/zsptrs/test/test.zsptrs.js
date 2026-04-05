/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zsptrs = require( './../lib/zsptrs.js' );


// TESTS //

test( 'zsptrs is a function', function t() {
	assert.strictEqual( typeof zsptrs, 'function', 'is a function' );
});

test( 'zsptrs has expected arity', function t() {
	assert.strictEqual( zsptrs.length, 8, 'has expected arity' );
});

test( 'zsptrs throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zsptrs( 'invalid', 'upper', new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zsptrs throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zsptrs( 'row-major', 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zsptrs throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zsptrs( 'row-major', 'upper', -1, 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'zsptrs throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		zsptrs( 'row-major', 'upper', new Float64Array( 4 ), -1, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, RangeError );
});
