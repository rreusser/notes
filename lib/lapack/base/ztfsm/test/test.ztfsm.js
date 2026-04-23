/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var ztfsm = require( './../lib/ztfsm.js' );


// TESTS //

test( 'ztfsm is a function', function t() {
	assert.strictEqual( typeof ztfsm, 'function', 'is a function' );
});

test( 'ztfsm has expected arity', function t() {
	assert.strictEqual( ztfsm.length, 10, 'has expected arity' );
});

test( 'ztfsm throws TypeError for invalid side', function t() {
	assert.throws( function throws() {
		ztfsm( 2, 'invalid', 'upper', 'no-transpose', 'non-unit', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'ztfsm throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		ztfsm( 2, 'left', 'invalid', 'no-transpose', 'non-unit', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'ztfsm throws TypeError for invalid trans', function t() {
	assert.throws( function throws() {
		ztfsm( 2, 'left', 'upper', 'invalid', 'non-unit', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'ztfsm throws TypeError for invalid diag', function t() {
	assert.throws( function throws() {
		ztfsm( 2, 'left', 'upper', 'no-transpose', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'ztfsm throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		ztfsm( 2, 'left', 'upper', 'no-transpose', 'non-unit', -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ) );
	}, RangeError );
});

test( 'ztfsm throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		ztfsm( 2, 'left', 'upper', 'no-transpose', 'non-unit', new Float64Array( 4 ), -1, 2, new Float64Array( 4 ), new Float64Array( 4 ) );
	}, RangeError );
});
