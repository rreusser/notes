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

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var dlarrf = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlarrf.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

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

var PIVMIN = 2.2250738585072014e-308; // DLAMCH('S')
var TOL = 1e-13;


// TESTS //

test( 'dlarrf: tridiag_4x4_full_cluster', function t() {
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
	var r = dlarrf( N, d, 1, 0, l, 1, 0, ld, 1, 0, 1, 4, w, 1, 0, wgap, 1, 0, werr, 1, 0, 4.0, 1.0, 1.0, PIVMIN, dplus, 1, 0, lplus, 1, 0, work, 1, 0 );
	assert.equal( r.info, tc.info, 'info' );
	assertClose( r.sigma, tc.sigma, TOL, 'sigma' );
	assertArrayClose( Array.from( dplus.slice( 0, N ) ), tc.dplus, TOL, 'dplus' );
	assertArrayClose( Array.from( lplus.slice( 0, N - 1 ) ), tc.lplus, TOL, 'lplus' );
});

test( 'dlarrf: tridiag_5x5_subset_cluster', function t() {
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
	var r = dlarrf( N, d, 1, 0, l, 1, 0, ld, 1, 0, 2, 4, w, 1, 0, wgap, 1, 0, werr, 1, 0, 5.0, 0.95, 0.95, PIVMIN, dplus, 1, 0, lplus, 1, 0, work, 1, 0 );
	assert.equal( r.info, tc.info, 'info' );
	assertClose( r.sigma, tc.sigma, TOL, 'sigma' );
	assertArrayClose( Array.from( dplus.slice( 0, N ) ), tc.dplus, TOL, 'dplus' );
	assertArrayClose( Array.from( lplus.slice( 0, N - 1 ) ), tc.lplus, TOL, 'lplus' );
});

test( 'dlarrf: tridiag_2x2', function t() {
	var tc = findCase( 'tridiag_2x2' );
	var N = 2;
	var d = new Float64Array( [ 2.0, 4.0 ] );
	var l = new Float64Array( [ 0.1, 0.0 ] );
	var ld = new Float64Array( [ 0.2, 0.0 ] );
	var w = new Float64Array( [ 1.99, 4.01 ] );
	var wgap = new Float64Array( [ 1.9, 0.0 ] );
	var werr = new Float64Array( [ 1e-5, 1e-5 ] );
	var dplus = new Float64Array( N );
	var lplus = new Float64Array( N );
	var work = new Float64Array( 2 * N );
	var r = dlarrf( N, d, 1, 0, l, 1, 0, ld, 1, 0, 1, 2, w, 1, 0, wgap, 1, 0, werr, 1, 0, 2.0, 1.0, 1.0, PIVMIN, dplus, 1, 0, lplus, 1, 0, work, 1, 0 );
	assert.equal( r.info, tc.info, 'info' );
	assertClose( r.sigma, tc.sigma, TOL, 'sigma' );
	assertArrayClose( Array.from( dplus.slice( 0, N ) ), tc.dplus, TOL, 'dplus' );
	assertArrayClose( Array.from( lplus.slice( 0, N - 1 ) ), tc.lplus, TOL, 'lplus' );
});

test( 'dlarrf: tight_cluster_6x6', function t() {
	var tc = findCase( 'tight_cluster_6x6' );
	var N = 6;
	var d = new Float64Array( [ 6.0, 5.0, 2.0, 2.000001, 2.000002, 1.0 ] );
	var l = new Float64Array( [ 0.01, 0.01, 0.01, 0.01, 0.01, 0.0 ] );
	var ld = new Float64Array( [ 0.06, 0.05, 0.02, 0.02000001, 0.02000002, 0.0 ] );
	var w = new Float64Array( [ 0.95, 1.999998, 1.999999, 2.000000, 2.000001, 6.05 ] );
	var wgap = new Float64Array( [ 1e-9, 1e-9, 1e-9, 1e-9, 4.0, 0.0 ] );
	var werr = new Float64Array( [ 1e-12, 1e-12, 1e-12, 1e-12, 1e-12, 1e-12 ] );
	var dplus = new Float64Array( N );
	var lplus = new Float64Array( N );
	var work = new Float64Array( 2 * N );
	var r = dlarrf( N, d, 1, 0, l, 1, 0, ld, 1, 0, 3, 5, w, 1, 0, wgap, 1, 0, werr, 1, 0, 6.0, 1.0, 4.0, PIVMIN, dplus, 1, 0, lplus, 1, 0, work, 1, 0 );
	assert.equal( r.info, tc.info, 'info' );
	assertClose( r.sigma, tc.sigma, TOL, 'sigma' );
	assertArrayClose( Array.from( dplus.slice( 0, N ) ), tc.dplus, TOL, 'dplus' );
	assertArrayClose( Array.from( lplus.slice( 0, N - 1 ) ), tc.lplus, TOL, 'lplus' );
});

test( 'dlarrf: tridiag_3x3_wide', function t() {
	var tc = findCase( 'tridiag_3x3_wide' );
	var N = 3;
	var d = new Float64Array( [ 10.0, 5.0, 1.0 ] );
	var l = new Float64Array( [ 0.2, 0.2, 0.0 ] );
	var ld = new Float64Array( [ 2.0, 1.0, 0.0 ] );
	var w = new Float64Array( [ 0.9, 4.9, 10.1 ] );
	var wgap = new Float64Array( [ 3.9, 5.0, 0.0 ] );
	var werr = new Float64Array( [ 1e-4, 1e-4, 1e-4 ] );
	var dplus = new Float64Array( N );
	var lplus = new Float64Array( N );
	var work = new Float64Array( 2 * N );
	var r = dlarrf( N, d, 1, 0, l, 1, 0, ld, 1, 0, 1, 3, w, 1, 0, wgap, 1, 0, werr, 1, 0, 10.0, 2.0, 2.0, PIVMIN, dplus, 1, 0, lplus, 1, 0, work, 1, 0 );
	assert.equal( r.info, tc.info, 'info' );
	assertClose( r.sigma, tc.sigma, TOL, 'sigma' );
	assertArrayClose( Array.from( dplus.slice( 0, N ) ), tc.dplus, TOL, 'dplus' );
	assertArrayClose( Array.from( lplus.slice( 0, N - 1 ) ), tc.lplus, TOL, 'lplus' );
});

test( 'dlarrf: n_zero quick return', function t() {
	var tc = findCase( 'n_zero' );
	var d = new Float64Array( 0 );
	var r = dlarrf( 0, d, 1, 0, d, 1, 0, d, 1, 0, 1, 1, d, 1, 0, d, 1, 0, d, 1, 0, 1.0, 1.0, 1.0, PIVMIN, d, 1, 0, d, 1, 0, d, 1, 0 );
	assert.equal( r.info, tc.info, 'info' );
	// JS impl returns sigma=0 on quick return
	assert.equal( r.sigma, 0.0, 'sigma' );
});


// WRAPPER TESTS //

test( 'main export is a function', function t() {
	var main = require( './../lib' );
	assert.strictEqual( typeof main, 'function', 'main export is a function' );
});

test( 'main export has an ndarray method', function t() {
	var main = require( './../lib' );
	assert.strictEqual( typeof main.ndarray, 'function', 'has ndarray method' );
});

test( 'dlarrf wrapper: tridiag_4x4_full_cluster (no offsets)', function t() {
	var tc = findCase( 'tridiag_4x4_full_cluster' );
	var main = require( './../lib' );
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
	var r = main( N, d, 1, l, 1, ld, 1, 1, 4, w, 1, wgap, 1, werr, 1, 4.0, 1.0, 1.0, PIVMIN, dplus, 1, lplus, 1, work, 1 );
	assert.equal( r.info, tc.info, 'info' );
	assertClose( r.sigma, tc.sigma, TOL, 'sigma' );
	assertArrayClose( Array.from( dplus.slice( 0, N ) ), tc.dplus, TOL, 'dplus' );
	assertArrayClose( Array.from( lplus.slice( 0, N - 1 ) ), tc.lplus, TOL, 'lplus' );
});

test( 'dlarrf wrapper: throws for negative N', function t() {
	var main = require( './../lib' );
	var d = new Float64Array( 0 );
	assert.throws( function bad() {
		main( -1, d, 1, d, 1, d, 1, 1, 1, d, 1, d, 1, d, 1, 1.0, 1.0, 1.0, PIVMIN, d, 1, d, 1, d, 1 );
	}, RangeError );
});

test( 'dlarrf.ndarray: tridiag_3x3_wide', function t() {
	var tc = findCase( 'tridiag_3x3_wide' );
	var main = require( './../lib' );
	var N = 3;
	var d = new Float64Array( [ 10.0, 5.0, 1.0 ] );
	var l = new Float64Array( [ 0.2, 0.2, 0.0 ] );
	var ld = new Float64Array( [ 2.0, 1.0, 0.0 ] );
	var w = new Float64Array( [ 0.9, 4.9, 10.1 ] );
	var wgap = new Float64Array( [ 3.9, 5.0, 0.0 ] );
	var werr = new Float64Array( [ 1e-4, 1e-4, 1e-4 ] );
	var dplus = new Float64Array( N );
	var lplus = new Float64Array( N );
	var work = new Float64Array( 2 * N );
	var r = main.ndarray( N, d, 1, 0, l, 1, 0, ld, 1, 0, 1, 3, w, 1, 0, wgap, 1, 0, werr, 1, 0, 10.0, 2.0, 2.0, PIVMIN, dplus, 1, 0, lplus, 1, 0, work, 1, 0 );
	assert.equal( r.info, tc.info, 'info' );
	assertClose( r.sigma, tc.sigma, TOL, 'sigma' );
	assertArrayClose( Array.from( dplus.slice( 0, N ) ), tc.dplus, TOL, 'dplus' );
	assertArrayClose( Array.from( lplus.slice( 0, N - 1 ) ), tc.lplus, TOL, 'lplus' );
});
