/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zpttrs = require( './../lib/zpttrs.js' );


// TESTS //

test( 'zpttrs is a function', function t() {
	assert.strictEqual( typeof zpttrs, 'function', 'is a function' );
});

test( 'zpttrs has expected arity', function t() {
	assert.strictEqual( zpttrs.length, 9, 'has expected arity' );
});

test( 'zpttrs throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zpttrs( 'invalid', new Float64Array( 4 ), 2, 2, 1, 2, 1, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zpttrs throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zpttrs( 'upper', -1, 2, 2, 1, 2, 1, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'zpttrs throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		zpttrs( 'upper', new Float64Array( 4 ), -1, 2, 1, 2, 1, new Float64Array( 4 ), 2 );
	}, RangeError );
});
