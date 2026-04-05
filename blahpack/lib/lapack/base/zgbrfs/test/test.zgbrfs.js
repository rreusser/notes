/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zgbrfs = require( './../lib/zgbrfs.js' );


// TESTS //

test( 'zgbrfs is a function', function t() {
	assert.strictEqual( typeof zgbrfs, 'function', 'is a function' );
});

test( 'zgbrfs has expected arity', function t() {
	assert.strictEqual( zgbrfs.length, 18, 'has expected arity' );
});

test( 'zgbrfs throws TypeError for invalid trans', function t() {
	assert.throws( function throws() {
		zgbrfs( 'invalid', new Float64Array( 4 ), 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'zgbrfs throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zgbrfs( 'no-transpose', -1, 2, 2, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ) );
	}, RangeError );
});

test( 'zgbrfs throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		zgbrfs( 'no-transpose', new Float64Array( 4 ), 2, 2, -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ) );
	}, RangeError );
});
