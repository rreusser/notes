/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zpbequ = require( './../lib/zpbequ.js' );


// TESTS //

test( 'zpbequ is a function', function t() {
	assert.strictEqual( typeof zpbequ, 'function', 'is a function' );
});

test( 'zpbequ has expected arity', function t() {
	assert.strictEqual( zpbequ.length, 7, 'has expected arity' );
});

test( 'zpbequ throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zpbequ( 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, 2, 1 );
	}, TypeError );
});

test( 'zpbequ throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zpbequ( 'upper', -1, 2, new Float64Array( 4 ), 2, 2, 1 );
	}, RangeError );
});
