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
var ndarrayFn = require( './../lib/ndarray.js' );


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


// VARIABLES //

var PIVMIN = 2.2250738585072014e-308;
var SENTINEL = -9.9e99;
var TOL = 1e-13;


// HELPERS //

// Embeds values into a padded Float64Array at a given offset with a given stride; remaining slots are filled with SENTINEL.
function padded( values, stride, offset, totalLen ) {
	var out = new Float64Array( totalLen );
	var i;
	for ( i = 0; i < totalLen; i++ ) {
		out[ i ] = SENTINEL;
	}
	for ( i = 0; i < values.length; i++ ) {
		out[ offset + ( i * stride ) ] = values[ i ];
	}
	return out;
}

function assertSentinelsIntact( arr, usedIdx, msg ) {
	var used = {};
	var i;
	for ( i = 0; i < usedIdx.length; i++ ) {
		used[ usedIdx[ i ] ] = true;
	}
	for ( i = 0; i < arr.length; i++ ) {
		if ( !used[ i ] ) {
			assert.equal( arr[ i ], SENTINEL, msg + ': sentinel at index ' + i + ' was modified (got ' + arr[ i ] + ')' );
		}
	}
}


// TESTS //

test( 'ndarray is a function', function t() {
	assert.strictEqual( typeof ndarrayFn, 'function', 'is a function' );
});

test( 'ndarray: tridiag_3x3_wide with stride=2 and nonzero offsets, sentinel padding preserved', function t() {
	var tc = findCase( 'tridiag_3x3_wide' );
	var d = padded( [ 10.0, 5.0, 1.0 ], 2, 1, 7 );
	var l = padded( [ 0.2, 0.2, 0.0 ], 2, 1, 7 );
	var ld = padded( [ 2.0, 1.0, 0.0 ], 2, 1, 7 );
	var w = padded( [ 0.9, 4.9, 10.1 ], 2, 1, 7 );
	var wgap = padded( [ 3.9, 5.0, 0.0 ], 2, 1, 7 );
	var werr = padded( [ 1e-4, 1e-4, 1e-4 ], 2, 1, 7 );
	var dplus = new Float64Array( 12 );
	var lplus = new Float64Array( 12 );
	var work = new Float64Array( 12 );
	var r;
	var i;
	for ( i = 0; i < dplus.length; i++ ) {
		dplus[ i ] = SENTINEL;
		lplus[ i ] = SENTINEL;
	}
	r = ndarrayFn( 3, d, 2, 1, l, 2, 1, ld, 2, 1, 1, 3, w, 2, 1, wgap, 2, 1, werr, 2, 1, 10.0, 2.0, 2.0, PIVMIN, dplus, 3, 2, lplus, 3, 2, work, 1, 0 );
	assert.equal( r.info, tc.info, 'info' );
	assertClose( r.sigma, tc.sigma, TOL, 'sigma' );
	for ( i = 0; i < 3; i++ ) {
		assertClose( dplus[ 2 + ( i * 3 ) ], tc.dplus[ i ], TOL, 'dplus[' + i + ']' );
	}
	for ( i = 0; i < 2; i++ ) {
		assertClose( lplus[ 2 + ( i * 3 ) ], tc.lplus[ i ], TOL, 'lplus[' + i + ']' );
	}
	assertSentinelsIntact( dplus, [ 2, 5, 8 ], 'dplus' );
	assertSentinelsIntact( lplus, [ 2, 5 ], 'lplus' );
});

test( 'ndarray: tridiag_4x4_full_cluster with negative strides for inputs', function t() {
	var tc = findCase( 'tridiag_4x4_full_cluster' );
	var d = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
	var l = new Float64Array( [ 0.0, 0.1, 0.1, 0.1 ] );
	var ld = new Float64Array( [ 0.0, 0.2, 0.3, 0.4 ] );
	var w = new Float64Array( [ 4.05, 2.95, 1.95, 0.95 ] );
	var wgap = new Float64Array( [ 0.0, 1.0, 0.9, 0.9 ] );
	var werr = new Float64Array( [ 1e-3, 1e-3, 1e-3, 1e-3 ] );
	var dplus = new Float64Array( 4 );
	var lplus = new Float64Array( 4 );
	var work = new Float64Array( 8 );
	var r;
	var i;
	r = ndarrayFn( 4, d, -1, 3, l, -1, 3, ld, -1, 3, 1, 4, w, -1, 3, wgap, -1, 3, werr, 1, 0, 4.0, 1.0, 1.0, PIVMIN, dplus, 1, 0, lplus, 1, 0, work, 1, 0 );
	assert.equal( r.info, tc.info, 'info' );
	assertClose( r.sigma, tc.sigma, TOL, 'sigma' );
	for ( i = 0; i < 4; i++ ) {
		assertClose( dplus[ i ], tc.dplus[ i ], TOL, 'dplus[' + i + ']' );
	}
	for ( i = 0; i < 3; i++ ) {
		assertClose( lplus[ i ], tc.lplus[ i ], TOL, 'lplus[' + i + ']' );
	}
});

test( 'ndarray: N=0 quick return does not touch output arrays', function t() {
	var tc = findCase( 'n_zero' );
	var d = new Float64Array( 0 );
	var dplus = new Float64Array( [ SENTINEL, SENTINEL ] );
	var lplus = new Float64Array( [ SENTINEL, SENTINEL ] );
	var work = new Float64Array( 0 );
	var r = ndarrayFn( 0, d, 1, 0, d, 1, 0, d, 1, 0, 1, 1, d, 1, 0, d, 1, 0, d, 1, 0, 1.0, 1.0, 1.0, PIVMIN, dplus, 1, 0, lplus, 1, 0, work, 1, 0 );
	assert.equal( r.info, tc.info, 'info' );
	assert.equal( dplus[ 0 ], SENTINEL, 'dplus[0] untouched' );
	assert.equal( dplus[ 1 ], SENTINEL, 'dplus[1] untouched' );
	assert.equal( lplus[ 0 ], SENTINEL, 'lplus[0] untouched' );
	assert.equal( lplus[ 1 ], SENTINEL, 'lplus[1] untouched' );
});
