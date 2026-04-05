/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zpoequ = require( './../lib/zpoequ.js' );


// TESTS //

test( 'zpoequ is a function', function t() {
	assert.strictEqual( typeof zpoequ, 'function', 'is a function' );
});

test( 'zpoequ has expected arity', function t() {
	assert.strictEqual( zpoequ.length, 5, 'has expected arity' );
});

test( 'zpoequ throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zpoequ( -1, new Float64Array( 4 ), 2, 2, 1 );
	}, RangeError );
});
