/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zhbev = require( './../lib/zhbev.js' );


// TESTS //

test( 'zhbev is a function', function t() {
	assert.strictEqual( typeof zhbev, 'function', 'is a function' );
});

test( 'zhbev has expected arity', function t() {
	assert.strictEqual( zhbev.length, 15, 'has expected arity' );
});

test( 'zhbev throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zhbev( 'invalid', 2, 'upper', new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zhbev throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zhbev( 'row-major', 2, 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zhbev throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zhbev( 'row-major', 2, 'upper', -1, 2, new Float64Array( 4 ), 2, 2, 1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
