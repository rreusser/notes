/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zhesv = require( './../lib/zhesv.js' );


// TESTS //

test( 'zhesv is a function', function t() {
	assert.strictEqual( typeof zhesv, 'function', 'is a function' );
});

test( 'zhesv has expected arity', function t() {
	assert.strictEqual( zhesv.length, 12, 'has expected arity' );
});

test( 'zhesv throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zhesv( 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2 );
	}, TypeError );
});

test( 'zhesv throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zhesv( 'upper', -1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2 );
	}, RangeError );
});

test( 'zhesv throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		zhesv( 'upper', new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2 );
	}, RangeError );
});
