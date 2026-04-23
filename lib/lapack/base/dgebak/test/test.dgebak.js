/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dgebak = require( './../lib/dgebak.js' );


// TESTS //

test( 'dgebak is a function', function t() {
	assert.strictEqual( typeof dgebak, 'function', 'is a function' );
});

test( 'dgebak has expected arity', function t() {
	assert.strictEqual( dgebak.length, 10, 'has expected arity' );
});

test( 'dgebak throws TypeError for invalid side', function t() {
	assert.throws( function throws() {
		dgebak( 2, 'invalid', new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dgebak throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dgebak( 2, 'left', -1, 2, 2, new Float64Array( 4 ), 1, new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, RangeError );
});

test( 'dgebak throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dgebak( 2, 'left', new Float64Array( 4 ), 2, 2, new Float64Array( 4 ), 1, -1, new Float64Array( 4 ), 2 );
	}, RangeError );
});
