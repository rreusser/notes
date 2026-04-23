/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var ilazlc = require( './../lib/ilazlc.js' );


// TESTS //

test( 'ilazlc is a function', function t() {
	assert.strictEqual( typeof ilazlc, 'function', 'is a function' );
});

test( 'ilazlc has expected arity', function t() {
	assert.strictEqual( ilazlc.length, 5, 'has expected arity' );
});

test( 'ilazlc throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		ilazlc( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'ilazlc throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		ilazlc( 'row-major', -1, new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'ilazlc throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		ilazlc( 'row-major', new Float64Array( 4 ), -1, new Float64Array( 4 ), 2 );
	}, RangeError );
});
