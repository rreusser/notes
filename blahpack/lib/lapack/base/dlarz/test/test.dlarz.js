/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlarz = require( './../lib/dlarz.js' );


// TESTS //

test( 'dlarz is a function', function t() {
	assert.strictEqual( typeof dlarz, 'function', 'is a function' );
});

test( 'dlarz has expected arity', function t() {
	assert.strictEqual( dlarz.length, 12, 'has expected arity' );
});

test( 'dlarz throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dlarz( 'invalid', 'left', 2, 2, 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dlarz throws TypeError for invalid side', function t() {
	assert.throws( function throws() {
		dlarz( 'row-major', 'invalid', 2, 2, 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dlarz throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dlarz( 'row-major', 'left', -1, 2, 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'dlarz throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlarz( 'row-major', 'left', 2, -1, 2, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});
