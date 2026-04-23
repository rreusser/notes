/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zhpev = require( './../lib/zhpev.js' );


// TESTS //

test( 'zhpev is a function', function t() {
	assert.strictEqual( typeof zhpev, 'function', 'is a function' );
});

test( 'zhpev has expected arity', function t() {
	assert.strictEqual( zhpev.length, 10, 'has expected arity' );
});

test( 'zhpev throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zhpev( 'invalid', 2, 'upper', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'zhpev throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zhpev( 'row-major', 2, 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'zhpev throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zhpev( 'row-major', 2, 'upper', -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ) );
	}, RangeError );
});
