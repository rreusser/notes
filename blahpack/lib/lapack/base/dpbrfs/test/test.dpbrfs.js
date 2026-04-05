/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dpbrfs = require( './../lib/dpbrfs.js' );


// TESTS //

test( 'dpbrfs is a function', function t() {
	assert.strictEqual( typeof dpbrfs, 'function', 'is a function' );
});

test( 'dpbrfs has expected arity', function t() {
	assert.strictEqual( dpbrfs.length, 16, 'has expected arity' );
});

test( 'dpbrfs throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dpbrfs( 'invalid', new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'dpbrfs throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dpbrfs( 'upper', -1, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ) );
	}, RangeError );
});

test( 'dpbrfs throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		dpbrfs( 'upper', new Float64Array( 4 ), 2, -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ) );
	}, RangeError );
});
