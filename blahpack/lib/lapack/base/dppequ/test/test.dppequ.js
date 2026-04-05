/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dppequ = require( './../lib/dppequ.js' );


// TESTS //

test( 'dppequ is a function', function t() {
	assert.strictEqual( typeof dppequ, 'function', 'is a function' );
});

test( 'dppequ has expected arity', function t() {
	assert.strictEqual( dppequ.length, 4, 'has expected arity' );
});

test( 'dppequ throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		dppequ( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2 );
	}, TypeError );
});

test( 'dppequ throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dppequ( 'upper', -1, new Float64Array( 4 ), 2 );
	}, RangeError );
});
