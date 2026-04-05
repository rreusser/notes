/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlansp = require( './../lib/zlansp.js' );


// TESTS //

test( 'zlansp is a function', function t() {
	assert.strictEqual( typeof zlansp, 'function', 'is a function' );
});

test( 'zlansp has expected arity', function t() {
	assert.strictEqual( zlansp.length, 5, 'has expected arity' );
});

test( 'zlansp throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zlansp( 2, 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'zlansp throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zlansp( 2, 'upper', -1, new Float64Array( 4 ), new Float64Array( 4 ) );
	}, RangeError );
});
