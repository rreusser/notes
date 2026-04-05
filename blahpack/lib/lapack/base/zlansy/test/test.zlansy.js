/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlansy = require( './../lib/zlansy.js' );


// TESTS //

test( 'zlansy is a function', function t() {
	assert.strictEqual( typeof zlansy, 'function', 'is a function' );
});

test( 'zlansy has expected arity', function t() {
	assert.strictEqual( zlansy.length, 7, 'has expected arity' );
});

test( 'zlansy throws TypeError for invalid norm', function t() {
	assert.throws( function throws() {
		zlansy( 'invalid', 'upper', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zlansy throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zlansy( 'max', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zlansy throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zlansy( 'max', 'upper', -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});
