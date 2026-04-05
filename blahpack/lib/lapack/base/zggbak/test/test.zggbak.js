/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zggbak = require( './../lib/zggbak.js' );


// TESTS //

test( 'zggbak is a function', function t() {
	assert.strictEqual( typeof zggbak, 'function', 'is a function' );
});

test( 'zggbak has expected arity', function t() {
	assert.strictEqual( zggbak.length, 13, 'has expected arity' );
});

test( 'zggbak throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zggbak( 'invalid', 2, 'left', new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zggbak throws TypeError for invalid side', function t() {
	assert.throws( function throws() {
		zggbak( 'row-major', 2, 'invalid', new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'zggbak throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zggbak( 'row-major', 2, 'left', -1, 2, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'zggbak throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		zggbak( 'row-major', 2, 'left', new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), 1, -1, new Float64Array( 4 ), 2 );
	}, RangeError );
});
