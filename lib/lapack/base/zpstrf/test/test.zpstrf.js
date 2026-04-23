/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zpstrf = require( './../lib/zpstrf.js' );


// TESTS //

test( 'zpstrf is a function', function t() {
	assert.strictEqual( typeof zpstrf, 'function', 'is a function' );
});

test( 'zpstrf has expected arity', function t() {
	assert.strictEqual( zpstrf.length, 9, 'has expected arity' );
});

test( 'zpstrf throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zpstrf( 'invalid', 'upper', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ) );
	}, TypeError );
});

test( 'zpstrf throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zpstrf( 'row-major', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ) );
	}, TypeError );
});

test( 'zpstrf throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zpstrf( 'row-major', 'upper', -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ) );
	}, RangeError );
});
