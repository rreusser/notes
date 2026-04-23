/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zhbgvx = require( './../lib/zhbgvx.js' );


// TESTS //

test( 'zhbgvx is a function', function t() {
	assert.strictEqual( typeof zhbgvx, 'function', 'is a function' );
});

test( 'zhbgvx has expected arity', function t() {
	assert.strictEqual( zhbgvx.length, 30, 'has expected arity' );
});

test( 'zhbgvx throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zhbgvx( 2, 2, 'invalid', new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 2, 2, 2, 2, 2, 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zhbgvx throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zhbgvx( 2, 2, 'upper', -1, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 2, 2, 2, 2, 2, 2, 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
