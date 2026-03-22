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
var base = require( './../lib/base.js' );

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dladiv.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );

// HELPERS //

function assertClose( actual, expected, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= 1e-14, msg + ': expected ' + expected + ', got ' + actual );
}

// TESTS //

test( 'dladiv: main export is a function', function t() {
	assert.strictEqual( typeof base, 'function' );
});

test( 'dladiv: (4+2i)/(1+1i) = 3-1i', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'dladiv_basic'; } );
	var out = new Float64Array( 2 );
	base( 4.0, 2.0, 1.0, 1.0, out );
	assertClose( out[ 0 ], tc.p, 'dladiv_basic p' );
	assertClose( out[ 1 ], tc.q, 'dladiv_basic q' );
});

test( 'dladiv: (1+0i)/(0+1i) = 0-1i', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'dladiv_pure_imag_denom'; } );
	var out = new Float64Array( 2 );
	base( 1.0, 0.0, 0.0, 1.0, out );
	assertClose( out[ 0 ], tc.p, 'dladiv_pure_imag_denom p' );
	assertClose( out[ 1 ], tc.q, 'dladiv_pure_imag_denom q' );
});

test( 'dladiv: (1+0i)/(1+0i) = 1+0i', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'dladiv_real_div'; } );
	var out = new Float64Array( 2 );
	base( 1.0, 0.0, 1.0, 0.0, out );
	assertClose( out[ 0 ], tc.p, 'dladiv_real_div p' );
	assert.strictEqual( out[ 1 ], tc.q, 'dladiv_real_div q' );
});

test( 'dladiv: (0+0i)/(1+1i) = 0+0i', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'dladiv_zero_numer'; } );
	var out = new Float64Array( 2 );
	base( 0.0, 0.0, 1.0, 1.0, out );
	assert.strictEqual( out[ 0 ], tc.p, 'dladiv_zero_numer p' );
	assert.strictEqual( out[ 1 ], tc.q, 'dladiv_zero_numer q' );
});

test( 'dladiv: (3+4i)/(1-2i) = -1+2i', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'dladiv_neg_denom'; } );
	var out = new Float64Array( 2 );
	base( 3.0, 4.0, 1.0, -2.0, out );
	assertClose( out[ 0 ], tc.p, 'dladiv_neg_denom p' );
	assertClose( out[ 1 ], tc.q, 'dladiv_neg_denom q' );
});

test( 'dladiv: large values (overflow-safe)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'dladiv_large'; } );
	var out = new Float64Array( 2 );
	base( 1.0e300, 1.0e300, 1.0e300, 1.0e300, out );
	assertClose( out[ 0 ], tc.p, 'dladiv_large p' );
	assertClose( out[ 1 ], tc.q, 'dladiv_large q' );
});

test( 'dladiv: near-overflow numerator (ab >= HALF*ov)', function t() {
	// (1e308 + 0i) / (1 + 0i) = 1e308 + 0i
	// ab = 1e308 >= HALF*ov (~9e307) → triggers numerator scaling
	var out = new Float64Array( 2 );
	base( 1.0e308, 0.0, 1.0, 0.0, out );
	assertClose( out[ 0 ], 1e308, 'near-overflow num p' );
	assertClose( out[ 1 ], 0.0, 'near-overflow num q' );
});

test( 'dladiv: near-overflow denominator (cd >= HALF*ov)', function t() {
	// (1 + 0i) / (1e308 + 0i) = 1e-308 + 0i
	// cd = 1e308 >= HALF*ov → triggers denominator scaling
	var out = new Float64Array( 2 );
	base( 1.0, 0.0, 1.0e308, 0.0, out );
	assertClose( out[ 0 ], 1e-308, 'near-overflow denom p' );
	assertClose( out[ 1 ], 0.0, 'near-overflow denom q' );
});

test( 'dladiv: small values', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'dladiv_small'; } );
	var out = new Float64Array( 2 );
	base( 1.0e-300, 1.0e-300, 1.0e-300, 1.0e-300, out );
	assertClose( out[ 0 ], tc.p, 'dladiv_small p' );
	assertClose( out[ 1 ], tc.q, 'dladiv_small q' );
});
