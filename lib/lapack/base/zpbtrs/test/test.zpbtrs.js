/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zpbtrs = require( './../lib/zpbtrs.js' );


// TESTS //

test( 'zpbtrs is a function', function t() {
	assert.strictEqual( typeof zpbtrs, 'function', 'is a function' );
});

test( 'zpbtrs has expected arity', function t() {
	assert.strictEqual( zpbtrs.length, 9, 'has expected arity' );
});

test( 'zpbtrs throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zpbtrs( 'invalid', 'upper', new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zpbtrs throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zpbtrs( 'row-major', 'invalid', new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zpbtrs throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zpbtrs( 'row-major', 'upper', -1, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'zpbtrs throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		zpbtrs( 'row-major', 'upper', new Float64Array( 4 ), 2, -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});
