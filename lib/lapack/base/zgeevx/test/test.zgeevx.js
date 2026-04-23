/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var zgeevx = require( './../lib/zgeevx.js' );


// TESTS //

test( 'zgeevx is a function', function t() {
	assert.strictEqual( typeof zgeevx, 'function', 'is a function' );
});
