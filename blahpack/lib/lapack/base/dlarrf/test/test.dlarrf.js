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

// Copyright (c) 2025 Ricky Reusser. Apache-2.0 License.

/* eslint-disable no-restricted-syntax, stdlib/first-unit-test, max-len */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var dlarrf = require( './../lib/dlarrf.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var fixture = readFileSync( path.join( fixtureDir, 'dlarrf.jsonl' ), 'utf8' )
	.trim().split( '\n' )
	.map( function parse( line ) { return JSON.parse( line ); } );

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual + ' (relErr=' + relErr + ')' );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}


// VARIABLES //

var PIVMIN = 2.2250738585072014e-308;
var TOL = 1e-13;


// TESTS //

test( 'dlarrf is a function', function t() {
	assert.strictEqual( typeof dlarrf, 'function', 'is a function' );
});

test( 'dlarrf has expected arity', function t() {
	assert.strictEqual( dlarrf.length, 25, 'has expected arity' );
});

test( 'dlarrf throws RangeError for negative N', function t() {
	var d = new Float64Array( 0 );
	assert.throws( function throws() {
		dlarrf( -1, d, 1, d, 1, d, 1, 1, 1, d, 1, d, 1, d, 1, 1.0, 1.0, 1.0, PIVMIN, d, 1, d, 1, d, 1 );
	}, RangeError );
});

test( 'dlarrf returns object with info and sigma for N=0 quick return', function t() {
	var tc = findCase( 'n_zero' );
	var d = new Float64Array( 0 );
	var r = dlarrf( 0, d, 1, d, 1, d, 1, 1, 1, d, 1, d, 1, d, 1, 1.0, 1.0, 1.0, PIVMIN, d, 1, d, 1, d, 1 );
	assert.equal( r.info, tc.info, 'info matches fixture' );
	assert.equal( typeof r.sigma, 'number', 'sigma is a number' );
});

test( 'dlarrf wrapper: tridiag_4x4_full_cluster matches fixture', function t() {
	var tc = findCase( 'tridiag_4x4_full_cluster' );
	var N = 4;
	var d = new Float64Array( [ 4.0, 3.0, 2.0, 1.0 ] );
	var l = new Float64Array( [ 0.1, 0.1, 0.1, 0.0 ] );
	var ld = new Float64Array( [ 0.4, 0.3, 0.2, 0.0 ] );
	var w = new Float64Array( [ 0.95, 1.95, 2.95, 4.05 ] );
	var wgap = new Float64Array( [ 0.9, 0.9, 1.0, 0.0 ] );
	var werr = new Float64Array( [ 1e-3, 1e-3, 1e-3, 1e-3 ] );
	var dplus = new Float64Array( N );
	var lplus = new Float64Array( N );
	var work = new Float64Array( 2 * N );
	var r = dlarrf( N, d, 1, l, 1, ld, 1, 1, 4, w, 1, wgap, 1, werr, 1, 4.0, 1.0, 1.0, PIVMIN, dplus, 1, lplus, 1, work, 1 );
	assert.equal( r.info, tc.info, 'info' );
	assertClose( r.sigma, tc.sigma, TOL, 'sigma' );
	assertArrayClose( Array.from( dplus.slice( 0, N ) ), tc.dplus, TOL, 'dplus' );
	assertArrayClose( Array.from( lplus.slice( 0, N - 1 ) ), tc.lplus, TOL, 'lplus' );
});

test( 'dlarrf wrapper: tridiag_5x5_subset_cluster matches fixture', function t() {
	var tc = findCase( 'tridiag_5x5_subset_cluster' );
	var N = 5;
	var d = new Float64Array( [ 5.0, 4.0, 3.0, 2.0, 1.0 ] );
	var l = new Float64Array( [ 0.05, 0.05, 0.05, 0.05, 0.0 ] );
	var ld = new Float64Array( [ 0.25, 0.20, 0.15, 0.10, 0.0 ] );
	var w = new Float64Array( [ 0.99, 1.99, 2.99, 3.99, 5.01 ] );
	var wgap = new Float64Array( [ 0.95, 0.95, 0.95, 0.95, 0.0 ] );
	var werr = new Float64Array( [ 1e-4, 1e-4, 1e-4, 1e-4, 1e-4 ] );
	var dplus = new Float64Array( N );
	var lplus = new Float64Array( N );
	var work = new Float64Array( 2 * N );
	var r = dlarrf( N, d, 1, l, 1, ld, 1, 2, 4, w, 1, wgap, 1, werr, 1, 5.0, 0.95, 0.95, PIVMIN, dplus, 1, lplus, 1, work, 1 );
	assert.equal( r.info, tc.info, 'info' );
	assertClose( r.sigma, tc.sigma, TOL, 'sigma' );
	assertArrayClose( Array.from( dplus.slice( 0, N ) ), tc.dplus, TOL, 'dplus' );
	assertArrayClose( Array.from( lplus.slice( 0, N - 1 ) ), tc.lplus, TOL, 'lplus' );
});
