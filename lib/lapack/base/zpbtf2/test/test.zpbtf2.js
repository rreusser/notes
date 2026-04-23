/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zpbtf2 = require( './../lib/zpbtf2.js' );


// TESTS //

test( 'zpbtf2 is a function', function t() {
	assert.strictEqual( typeof zpbtf2, 'function', 'is a function' );
});

test( 'zpbtf2 has expected arity', function t() {
	assert.strictEqual( zpbtf2.length, 6, 'has expected arity' );
});

test( 'zpbtf2 throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zpbtf2( 'invalid', 'upper', new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zpbtf2 throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zpbtf2( 'row-major', 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zpbtf2 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zpbtf2( 'row-major', 'upper', -1, 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});
