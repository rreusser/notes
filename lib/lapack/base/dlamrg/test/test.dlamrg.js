/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlamrg = require( './../lib/dlamrg.js' );


// TESTS //

test( 'dlamrg is a function', function t() {
	assert.strictEqual( typeof dlamrg, 'function', 'is a function' );
});

test( 'dlamrg has expected arity', function t() {
	assert.strictEqual( dlamrg.length, 8, 'has expected arity' );
});
