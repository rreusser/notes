/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zgtcon = require( './../lib/zgtcon.js' );


// TESTS //

test( 'zgtcon is a function', function t() {
	assert.strictEqual( typeof zgtcon, 'function', 'is a function' );
});

test( 'zgtcon has expected arity', function t() {
	assert.strictEqual( zgtcon.length, 16, 'has expected arity' );
});

test( 'zgtcon throws TypeError for invalid norm', function t() {
	assert.throws( function throws() {
		zgtcon( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 1, 2, 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, 2, 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zgtcon throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zgtcon( 'one-norm', -1, new Float64Array( 4 ), 1, 2, 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, 2, 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});
