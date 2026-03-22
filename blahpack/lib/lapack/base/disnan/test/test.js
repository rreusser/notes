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
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var disnan = require( './../lib' );
var base = require( './../lib/base.js' );

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'disnan.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );

// TESTS //

test( 'disnan: main export is a function', function t() {
	assert.strictEqual( typeof disnan, 'function' );
});

test( 'disnan: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof disnan.ndarray, 'function' );
});

test( 'disnan: returns true for NaN', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'disnan_nan'; } );
	var result = base( NaN );
	assert.strictEqual( result, tc.result === 1 );
});

test( 'disnan: returns false for zero', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'disnan_zero'; } );
	var result = base( 0.0 );
	assert.strictEqual( result, tc.result === 1 );
});

test( 'disnan: returns false for one', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'disnan_one'; } );
	var result = base( 1.0 );
	assert.strictEqual( result, tc.result === 1 );
});

test( 'disnan: returns false for Infinity', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'disnan_inf'; } );
	var result = base( Infinity );
	assert.strictEqual( result, tc.result === 1 );
});

test( 'disnan: returns false for -Infinity', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'disnan_neginf'; } );
	var result = base( -Infinity );
	assert.strictEqual( result, tc.result === 1 );
});

test( 'disnan: returns false for large values', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'disnan_large'; } );
	var result = base( 1.0e300 );
	assert.strictEqual( result, tc.result === 1 );
});
