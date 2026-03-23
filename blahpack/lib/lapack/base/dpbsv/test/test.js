

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var dpbsv = require( './../lib' );

test( 'dpbsv: main export is a function', function t() {
	assert.strictEqual( typeof dpbsv, 'function' );
});

test( 'dpbsv: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof dpbsv.ndarray, 'function' );
});

// TODO: Add implementation tests
