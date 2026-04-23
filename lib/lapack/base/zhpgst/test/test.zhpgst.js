/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zhpgst = require( './../lib/zhpgst.js' );


// TESTS //

test( 'zhpgst is a function', function t() {
	assert.strictEqual( typeof zhpgst, 'function', 'is a function' );
});

test( 'zhpgst has expected arity', function t() {
	assert.strictEqual( zhpgst.length, 5, 'has expected arity' );
});

test( 'zhpgst throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zhpgst( 2, 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'zhpgst throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zhpgst( 2, 'upper', -1, new Float64Array( 4 ), new Float64Array( 4 ) );
	}, RangeError );
});
