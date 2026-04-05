/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zppequ = require( './../lib/zppequ.js' );


// TESTS //

test( 'zppequ is a function', function t() {
	assert.strictEqual( typeof zppequ, 'function', 'is a function' );
});

test( 'zppequ has expected arity', function t() {
	assert.strictEqual( zppequ.length, 4, 'has expected arity' );
});

test( 'zppequ throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zppequ( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zppequ throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zppequ( 'upper', -1, new Float64Array( 4 ), 2 );
	}, RangeError );
});
