/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlaset = require( './../lib/zlaset.js' );


// TESTS //

test( 'zlaset is a function', function t() {
	assert.strictEqual( typeof zlaset, 'function', 'is a function' );
});

test( 'zlaset has expected arity', function t() {
	assert.strictEqual( zlaset.length, 8, 'has expected arity' );
});

test( 'zlaset throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zlaset( 'invalid', 'upper', new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zlaset throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zlaset( 'row-major', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zlaset throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		zlaset( 'row-major', 'upper', -1, new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'zlaset throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zlaset( 'row-major', 'upper', new Float64Array( 4 ), -1, 2, 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});
