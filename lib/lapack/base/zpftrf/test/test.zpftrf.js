/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zpftrf = require( './../lib/zpftrf.js' );


// TESTS //

test( 'zpftrf is a function', function t() {
	assert.strictEqual( typeof zpftrf, 'function', 'is a function' );
});

test( 'zpftrf has expected arity', function t() {
	assert.strictEqual( zpftrf.length, 4, 'has expected arity' );
});

test( 'zpftrf throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zpftrf( 2, 'invalid', new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'zpftrf throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zpftrf( 2, 'upper', -1, new Float64Array( 4 ) );
	}, RangeError );
});
