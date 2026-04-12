/**
* @license Apache-2.0
*
* Copyright (c) 2025 Ricky Reusser.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*    http://www.apache.org/licenses/LICENSE-2.0
*/

/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var dlartgs = require( './../lib/dlartgs.js' );


// TESTS //

test( 'dlartgs is a function', function t() {
	assert.strictEqual( typeof dlartgs, 'function', 'is a function' );
});

test( 'dlartgs has expected arity', function t() {
	assert.strictEqual( dlartgs.length, 3, 'has expected arity' );
});

test( 'dlartgs returns an object with cs and sn', function t() {
	var r = dlartgs( 3.0, 4.0, 1.5 );
	assert.ok( typeof r.cs === 'number' );
	assert.ok( typeof r.sn === 'number' );
});
