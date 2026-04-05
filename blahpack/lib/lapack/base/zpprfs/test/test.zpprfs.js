/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zpprfs = require( './../lib/zpprfs.js' );


// TESTS //

test( 'zpprfs is a function', function t() {
	assert.strictEqual( typeof zpprfs, 'function', 'is a function' );
});

test( 'zpprfs has expected arity', function t() {
	assert.strictEqual( zpprfs.length, 13, 'has expected arity' );
});

test( 'zpprfs throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zpprfs( 'invalid', new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'zpprfs throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zpprfs( 'upper', -1, 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ) );
	}, RangeError );
});

test( 'zpprfs throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		zpprfs( 'upper', new Float64Array( 4 ), -1, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ) );
	}, RangeError );
});
