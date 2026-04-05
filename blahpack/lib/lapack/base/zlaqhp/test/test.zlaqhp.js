/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlaqhp = require( './../lib/zlaqhp.js' );


// TESTS //

test( 'zlaqhp is a function', function t() {
	assert.strictEqual( typeof zlaqhp, 'function', 'is a function' );
});

test( 'zlaqhp has expected arity', function t() {
	assert.strictEqual( zlaqhp.length, 7, 'has expected arity' );
});

test( 'zlaqhp throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zlaqhp( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 1, 2, 2 );
	}, TypeError );
});

test( 'zlaqhp throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zlaqhp( 'upper', -1, new Float64Array( 4 ), new Float64Array( 4 ), 1, 2, 2 );
	}, RangeError );
});
