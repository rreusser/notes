/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zhbgst = require( './../lib/zhbgst.js' );


// TESTS //

test( 'zhbgst is a function', function t() {
	assert.strictEqual( typeof zhbgst, 'function', 'is a function' );
});

test( 'zhbgst has expected arity', function t() {
	assert.strictEqual( zhbgst.length, 13, 'has expected arity' );
});

test( 'zhbgst throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zhbgst( 2, 'invalid', new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'zhbgst throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zhbgst( 2, 'upper', -1, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ) );
	}, RangeError );
});
