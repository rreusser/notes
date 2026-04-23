/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zpbsv = require( './../lib/zpbsv.js' );


// TESTS //

test( 'zpbsv is a function', function t() {
	assert.strictEqual( typeof zpbsv, 'function', 'is a function' );
});

test( 'zpbsv has expected arity', function t() {
	assert.strictEqual( zpbsv.length, 8, 'has expected arity' );
});

test( 'zpbsv throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zpbsv( 'invalid', new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zpbsv throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zpbsv( 'upper', -1, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'zpbsv throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		zpbsv( 'upper', new Float64Array( 4 ), 2, -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});
