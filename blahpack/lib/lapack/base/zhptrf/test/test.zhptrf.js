/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zhptrf = require( './../lib/zhptrf.js' );


// TESTS //

test( 'zhptrf is a function', function t() {
	assert.strictEqual( typeof zhptrf, 'function', 'is a function' );
});

test( 'zhptrf has expected arity', function t() {
	assert.strictEqual( zhptrf.length, 4, 'has expected arity' );
});

test( 'zhptrf throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zhptrf( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'zhptrf throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zhptrf( 'upper', -1, new Float64Array( 4 ), new Float64Array( 4 ) );
	}, RangeError );
});
