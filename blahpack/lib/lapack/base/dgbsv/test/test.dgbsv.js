/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dgbsv = require( './../lib/dgbsv.js' );


// TESTS //

test( 'dgbsv is a function', function t() {
	assert.strictEqual( typeof dgbsv, 'function', 'is a function' );
});

test( 'dgbsv has expected arity', function t() {
	assert.strictEqual( dgbsv.length, 11, 'has expected arity' );
});

test( 'dgbsv throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dgbsv( 'invalid', new Float64Array( 4 ), 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dgbsv throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dgbsv( 'row-major', -1, 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'dgbsv throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		dgbsv( 'row-major', new Float64Array( 4 ), 2, 2, -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 2 );
	}, RangeError );
});
