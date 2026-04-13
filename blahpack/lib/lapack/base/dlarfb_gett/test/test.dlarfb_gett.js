/* eslint-disable no-restricted-syntax, stdlib/first-unit-test, camelcase */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlarfb_gett = require( './../lib/dlarfb_gett.js' );


// TESTS //

test( 'dlarfb_gett is a function', function t() {
	assert.strictEqual( typeof dlarfb_gett, 'function', 'is a function' );
});

test( 'dlarfb_gett has expected arity', function t() {
	assert.strictEqual( dlarfb_gett.length, 13, 'has expected arity' );
});

test( 'dlarfb_gett throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dlarfb_gett( 'invalid', 'not-identity', 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dlarfb_gett throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dlarfb_gett( 'row-major', 'not-identity', -1, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'dlarfb_gett throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlarfb_gett( 'row-major', 'not-identity', 2, -1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'dlarfb_gett throws RangeError for negative K', function t() {
	assert.throws( function throws() {
		dlarfb_gett( 'row-major', 'not-identity', 2, 2, -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});
