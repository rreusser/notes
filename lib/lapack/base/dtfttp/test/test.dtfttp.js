/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dtfttp = require( './../lib/dtfttp.js' );


// TESTS //

test( 'dtfttp is a function', function t() {
	assert.strictEqual( typeof dtfttp, 'function', 'is a function' );
});

test( 'dtfttp has expected arity', function t() {
	assert.strictEqual( dtfttp.length, 5, 'has expected arity' );
});

test( 'dtfttp throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dtfttp( 2, 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'dtfttp throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dtfttp( 2, 'upper', -1, new Float64Array( 4 ), new Float64Array( 4 ) );
	}, RangeError );
});
