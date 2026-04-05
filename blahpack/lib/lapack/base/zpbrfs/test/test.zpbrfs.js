/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zpbrfs = require( './../lib/zpbrfs.js' );


// TESTS //

test( 'zpbrfs is a function', function t() {
	assert.strictEqual( typeof zpbrfs, 'function', 'is a function' );
});

test( 'zpbrfs has expected arity', function t() {
	assert.strictEqual( zpbrfs.length, 16, 'has expected arity' );
});

test( 'zpbrfs throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zpbrfs( 'invalid', new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'zpbrfs throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zpbrfs( 'upper', -1, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ) );
	}, RangeError );
});

test( 'zpbrfs throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		zpbrfs( 'upper', new Float64Array( 4 ), 2, -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ) );
	}, RangeError );
});
