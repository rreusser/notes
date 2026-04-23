/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zppcon = require( './../lib/zppcon.js' );


// TESTS //

test( 'zppcon is a function', function t() {
	assert.strictEqual( typeof zppcon, 'function', 'is a function' );
});

test( 'zppcon has expected arity', function t() {
	assert.strictEqual( zppcon.length, 7, 'has expected arity' );
});

test( 'zppcon throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zppcon( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'zppcon throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zppcon( 'upper', -1, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), new Float64Array( 4 ) );
	}, RangeError );
});
