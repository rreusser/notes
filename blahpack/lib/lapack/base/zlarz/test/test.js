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

/* eslint-disable no-restricted-syntax, stdlib/first-unit-test, max-len */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zlarz = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zlarz.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}


// TESTS //

test( 'main export is a function', function t() {
	assert.strictEqual( typeof zlarz, 'function', 'main export is a function' );
});

test( 'zlarz: left side, M=4, N=3, L=2', function t() {
	var tc = findCase( 'zlarz_left_4x3_l2' );
	var v = new Complex128Array( [ 0.5, 0.2, -0.3, 0.4 ] );
	var tau = new Complex128Array( [ 1.2, -0.4 ] );
	var C = new Complex128Array( [
		1.0, 0.0, 2.0, 1.0, 3.0, -1.0, 4.0, 0.5,
		-1.0, 2.0, 0.5, 0.5, 1.5, -0.5, -2.0, 1.0,
		0.0, 1.0, 1.0, 1.0, -0.5, 0.0, 2.0, -2.0
	] );
	var WORK = new Complex128Array( 3 );
	zlarz( 'left', 4, 3, 2, v, 1, 0, tau, 0, C, 1, 4, 0, WORK, 1, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.C, 1e-13, 'C' );
});

test( 'zlarz: right side, M=3, N=4, L=2', function t() {
	var tc = findCase( 'zlarz_right_3x4_l2' );
	var v = new Complex128Array( [ 0.5, 0.2, -0.3, 0.4 ] );
	var tau = new Complex128Array( [ 0.8, 0.5 ] );
	var C = new Complex128Array( [
		1.0, 0.0, 2.0, 1.0, 3.0, -1.0,
		4.0, 0.5, -1.0, 2.0, 0.5, 0.5,
		1.5, -0.5, -2.0, 1.0, 0.0, 1.0,
		1.0, 1.0, -0.5, 0.0, 2.0, -2.0
	] );
	var WORK = new Complex128Array( 3 );
	zlarz( 'right', 3, 4, 2, v, 1, 0, tau, 0, C, 1, 3, 0, WORK, 1, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.C, 1e-13, 'C' );
});

test( 'zlarz: tau = 0 is a no-op', function t() {
	var tc = findCase( 'zlarz_tau_zero' );
	var v = new Complex128Array( [ 0.5, 0.2, -0.3, 0.4 ] );
	var tau = new Complex128Array( [ 0.0, 0.0 ] );
	var C = new Complex128Array( [
		1.0, 0.0, 2.0, 1.0, 3.0, -1.0,
		4.0, 0.5, -1.0, 2.0, 0.5, 0.5
	] );
	var WORK = new Complex128Array( 2 );
	zlarz( 'left', 3, 2, 2, v, 1, 0, tau, 0, C, 1, 3, 0, WORK, 1, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.C, 1e-14, 'C' );
});

test( 'zlarz: L=0 (only first row updated by -tau*C(1,:))', function t() {
	var tc = findCase( 'zlarz_l_zero_left' );
	var v = new Complex128Array( [ 0.5, 0.2, -0.3, 0.4 ] );
	var tau = new Complex128Array( [ 1.0, 0.3 ] );
	var C = new Complex128Array( [
		1.0, 0.0, 2.0, 1.0, 3.0, -1.0,
		4.0, 0.5, -1.0, 2.0, 0.5, 0.5
	] );
	var WORK = new Complex128Array( 2 );
	zlarz( 'left', 3, 2, 0, v, 1, 0, tau, 0, C, 1, 3, 0, WORK, 1, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.C, 1e-14, 'C' );
});

test( 'zlarz: left side, M=3, N=2, L=3 (L == M)', function t() {
	var tc = findCase( 'zlarz_left_3x2_l3' );
	var v = new Complex128Array( [ 0.5, 0.2, -0.3, 0.4, 0.7, -0.1 ] );
	var tau = new Complex128Array( [ 0.9, 0.2 ] );
	var C = new Complex128Array( [
		1.0, 0.0, 2.0, 1.0, 3.0, -1.0,
		4.0, 0.5, -1.0, 2.0, 0.5, 0.5
	] );
	var WORK = new Complex128Array( 2 );
	zlarz( 'left', 3, 2, 3, v, 1, 0, tau, 0, C, 1, 3, 0, WORK, 1, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.C, 1e-13, 'C' );
});

test( 'zlarz: right side, M=2, N=3, L=3 (L == N)', function t() {
	var tc = findCase( 'zlarz_right_2x3_l3' );
	var v = new Complex128Array( [ 0.5, 0.2, -0.3, 0.4, 0.7, -0.1 ] );
	var tau = new Complex128Array( [ 1.1, -0.3 ] );
	var C = new Complex128Array( [
		1.0, 0.0, 2.0, 1.0,
		3.0, -1.0, 4.0, 0.5,
		-1.0, 2.0, 0.5, 0.5
	] );
	var WORK = new Complex128Array( 2 );
	zlarz( 'right', 2, 3, 3, v, 1, 0, tau, 0, C, 1, 2, 0, WORK, 1, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.C, 1e-13, 'C' );
});
