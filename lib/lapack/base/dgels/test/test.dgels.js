/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dgels = require( './../lib/dgels.js' );


// TESTS //

test( 'dgels is a function', function t() {
	assert.strictEqual( typeof dgels, 'function', 'is a function' );
});

test( 'dgels has expected arity', function t() {
	assert.strictEqual( dgels.length, 9, 'has expected arity' );
});

test( 'dgels throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dgels( 'invalid', 'no-transpose', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dgels throws TypeError for invalid trans', function t() {
	assert.throws( function throws() {
		dgels( 'row-major', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dgels throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dgels( 'row-major', 'no-transpose', -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'dgels throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dgels( 'row-major', 'no-transpose', new Float64Array( 4 ), -1, 2, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'dgels throws RangeError for negative nrhs', function t() {
	assert.throws( function throws() {
		dgels( 'row-major', 'no-transpose', new Float64Array( 4 ), new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 2 );
	}, RangeError );
});
