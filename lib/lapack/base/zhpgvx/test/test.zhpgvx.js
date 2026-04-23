/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zhpgvx = require( './../lib/zhpgvx.js' );


// TESTS //

test( 'zhpgvx is a function', function t() {
	assert.strictEqual( typeof zhpgvx, 'function', 'is a function' );
});

test( 'zhpgvx has expected arity', function t() {
	assert.strictEqual( zhpgvx.length, 21, 'has expected arity' );
});

test( 'zhpgvx throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zhpgvx( 'invalid', 2, 2, 2, 'upper', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 2, 2, 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'zhpgvx throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zhpgvx( 'row-major', 2, 2, 2, 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 2, 2, 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'zhpgvx throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zhpgvx( 'row-major', 2, 2, 2, 'upper', -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 2, 2, 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ) );
	}, RangeError );
});
