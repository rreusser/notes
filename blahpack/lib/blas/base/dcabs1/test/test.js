/* eslint-disable no-restricted-syntax, stdlib/require-globals, stdlib/first-unit-test */

/**
* @license Apache-2.0
*
* Copyright (c) 2025 The Stdlib Authors.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*    http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/

'use strict';

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dcabs1 = require( './../lib' );
var base = require( './../lib/base.js' );

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dcabs1.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// TESTS //

test( 'dcabs1: computes |Re(z)| + |Im(z)| for (3+4i)', function t() {
	var tc = fixture.find( function ( t ) { return t.name === 'basic'; } );
	assert.equal( base( new Float64Array( [ 3, 4 ] ) ), tc.result );
});

test( 'dcabs1: returns 0 for (0+0i)', function t() {
	var tc = fixture.find( function ( t ) { return t.name === 'zero'; } );
	assert.equal( base( new Float64Array( [ 0, 0 ] ) ), tc.result );
});

test( 'dcabs1: handles negative components', function t() {
	var tc = fixture.find( function ( t ) { return t.name === 'negative'; } );
	assert.equal( base( new Float64Array( [ -5, 12 ] ) ), tc.result );
});
