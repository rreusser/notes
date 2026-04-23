/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dpbsv = require( './../lib/dpbsv.js' );


// TESTS //

test( 'dpbsv is a function', function t() {
	assert.strictEqual( typeof dpbsv, 'function', 'is a function' );
});

test( 'dpbsv has expected arity', function t() {
	assert.strictEqual( dpbsv.length, 9, 'has expected arity' );
});

test( 'dpbsv throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dpbsv( 'invalid', 'upper', new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dpbsv throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dpbsv( 'row-major', 'invalid', new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dpbsv throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dpbsv( 'row-major', 'upper', -1, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'dpbsv throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		dpbsv( 'row-major', 'upper', new Float64Array( 4 ), 2, -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});
