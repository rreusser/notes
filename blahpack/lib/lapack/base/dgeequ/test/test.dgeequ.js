/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dgeequ = require( './../lib/dgeequ.js' );


// TESTS //

test( 'dgeequ is a function', function t() {
	assert.strictEqual( typeof dgeequ, 'function', 'is a function' );
});

test( 'dgeequ has expected arity', function t() {
	assert.strictEqual( dgeequ.length, 8, 'has expected arity' );
});

test( 'dgeequ throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dgeequ( -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 1, 2, 1 );
	}, RangeError );
});

test( 'dgeequ throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dgeequ( new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, 2, 1, 2, 1 );
	}, RangeError );
});
