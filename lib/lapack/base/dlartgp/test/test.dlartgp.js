/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var dlartgp = require( './../lib/dlartgp.js' );


// TESTS //

test( 'dlartgp is a function', function t() {
	assert.strictEqual( typeof dlartgp, 'function', 'is a function' );
});

test( 'dlartgp has expected arity', function t() {
	assert.strictEqual( dlartgp.length, 2, 'has expected arity' );
});
