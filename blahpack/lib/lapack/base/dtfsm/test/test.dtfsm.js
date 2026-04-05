/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dtfsm = require( './../lib/dtfsm.js' );


// TESTS //

test( 'dtfsm is a function', function t() {
	assert.strictEqual( typeof dtfsm, 'function', 'is a function' );
});

test( 'dtfsm has expected arity', function t() {
	assert.strictEqual( dtfsm.length, 11, 'has expected arity' );
});

test( 'dtfsm throws TypeError for invalid side', function t() {
	assert.throws( function throws() {
		dtfsm( 2, 'invalid', 'upper', 'no-transpose', 'non-unit', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dtfsm throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dtfsm( 2, 'left', 'invalid', 'no-transpose', 'non-unit', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dtfsm throws TypeError for invalid trans', function t() {
	assert.throws( function throws() {
		dtfsm( 2, 'left', 'upper', 'invalid', 'non-unit', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dtfsm throws TypeError for invalid diag', function t() {
	assert.throws( function throws() {
		dtfsm( 2, 'left', 'upper', 'no-transpose', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dtfsm throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dtfsm( 2, 'left', 'upper', 'no-transpose', 'non-unit', -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'dtfsm throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dtfsm( 2, 'left', 'upper', 'no-transpose', 'non-unit', new Float64Array( 4 ), -1, 2, new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, RangeError );
});
