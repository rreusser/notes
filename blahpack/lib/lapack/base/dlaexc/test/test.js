

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var dlaexc = require( './../lib' );

test( 'dlaexc: main export is a function', function t() {
	assert.strictEqual( typeof dlaexc, 'function' );
});

test( 'dlaexc: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof dlaexc.ndarray, 'function' );
});

// TODO: Add implementation tests
