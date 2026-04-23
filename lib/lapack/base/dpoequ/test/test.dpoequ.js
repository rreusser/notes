/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dpoequ = require( './../lib/dpoequ.js' );


// TESTS //

test( 'dpoequ is a function', function t() {
	assert.strictEqual( typeof dpoequ, 'function', 'is a function' );
});

test( 'dpoequ has expected arity', function t() {
	assert.strictEqual( dpoequ.length, 5, 'has expected arity' );
});

test( 'dpoequ throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dpoequ( -1, new Float64Array( 4 ), 2, 2, 1 );
	}, RangeError );
});
