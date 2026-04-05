/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zungql = require( './../lib/zungql.js' );


// TESTS //

test( 'zungql is a function', function t() {
	assert.strictEqual( typeof zungql, 'function', 'is a function' );
});

test( 'zungql has expected arity', function t() {
	assert.strictEqual( zungql.length, 10, 'has expected arity' );
});

test( 'zungql throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zungql( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zungql throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		zungql( 'row-major', -1, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'zungql throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zungql( 'row-major', new Float64Array( 4 ), -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'zungql throws RangeError for negative K', function t() {
	assert.throws( function throws() {
		zungql( 'row-major', new Float64Array( 4 ), new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1 );
	}, RangeError );
});
