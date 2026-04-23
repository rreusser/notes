/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var dznrm2 = require( './../lib/base.js' );


// TESTS //

test( 'main export is a function', function t() {
	assert.strictEqual( typeof dznrm2, 'function', 'main export is a function' );
});

