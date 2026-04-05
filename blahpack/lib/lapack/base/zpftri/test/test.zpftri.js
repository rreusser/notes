/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zpftri = require( './../lib/zpftri.js' );


// TESTS //

test( 'zpftri is a function', function t() {
	assert.strictEqual( typeof zpftri, 'function', 'is a function' );
});

test( 'zpftri has expected arity', function t() {
	assert.strictEqual( zpftri.length, 4, 'has expected arity' );
});

test( 'zpftri throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zpftri( 2, 'invalid', new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'zpftri throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zpftri( 2, 'upper', -1, new Float64Array( 4 ) );
	}, RangeError );
});
