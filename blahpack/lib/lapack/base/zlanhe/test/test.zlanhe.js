/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlanhe = require( './../lib/zlanhe.js' );


// TESTS //

test( 'zlanhe is a function', function t() {
	assert.strictEqual( typeof zlanhe, 'function', 'is a function' );
});

test( 'zlanhe has expected arity', function t() {
	assert.strictEqual( zlanhe.length, 8, 'has expected arity' );
});

test( 'zlanhe throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zlanhe( 'invalid', 2, 'upper', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zlanhe throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zlanhe( 'row-major', 2, 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zlanhe throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zlanhe( 'row-major', 2, 'upper', -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});
