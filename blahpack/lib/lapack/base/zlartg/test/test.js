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
var lines = readFileSync( path.join( fixtureDir, 'zlartg.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );

// HELPERS //

function assertClose( actual, expected, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= 1e-14, msg + ': expected ' + expected + ', got ' + actual );
}

// TESTS //

test( 'zlartg: main export is a function', function t() {
	assert.strictEqual( typeof base, 'function' );
});

test( 'zlartg: g=0 => c=1, s=0, r=f', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlartg_g_zero'; } );
	var f = new Float64Array( [ 3.0, 4.0 ] );
	var g = new Float64Array( [ 0.0, 0.0 ] );
	var out = new Float64Array( 5 );
	base( f, g, out );
	assertClose( out[ 0 ], tc.c, 'zlartg_g_zero c' );
	assertClose( out[ 1 ], tc.s[ 0 ], 'zlartg_g_zero s[0]' );
	assertClose( out[ 2 ], tc.s[ 1 ], 'zlartg_g_zero s[1]' );
	assertClose( out[ 3 ], tc.r[ 0 ], 'zlartg_g_zero r[0]' );
	assertClose( out[ 4 ], tc.r[ 1 ], 'zlartg_g_zero r[1]' );
});

test( 'zlartg: f=0, g=(3+4i)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlartg_f_zero'; } );
	var f = new Float64Array( [ 0.0, 0.0 ] );
	var g = new Float64Array( [ 3.0, 4.0 ] );
	var out = new Float64Array( 5 );
	base( f, g, out );
	assertClose( out[ 0 ], tc.c, 'zlartg_f_zero c' );
	assertClose( out[ 1 ], tc.s[ 0 ], 'zlartg_f_zero s[0]' );
	assertClose( out[ 2 ], tc.s[ 1 ], 'zlartg_f_zero s[1]' );
	assertClose( out[ 3 ], tc.r[ 0 ], 'zlartg_f_zero r[0]' );
	assertClose( out[ 4 ], tc.r[ 1 ], 'zlartg_f_zero r[1]' );
});

test( 'zlartg: both real f=3, g=4', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlartg_real'; } );
	var f = new Float64Array( [ 3.0, 0.0 ] );
	var g = new Float64Array( [ 4.0, 0.0 ] );
	var out = new Float64Array( 5 );
	base( f, g, out );
	assertClose( out[ 0 ], tc.c, 'zlartg_real c' );
	assertClose( out[ 1 ], tc.s[ 0 ], 'zlartg_real s[0]' );
	assertClose( out[ 2 ], tc.s[ 1 ], 'zlartg_real s[1]' );
	assertClose( out[ 3 ], tc.r[ 0 ], 'zlartg_real r[0]' );
	assertClose( out[ 4 ], tc.r[ 1 ], 'zlartg_real r[1]' );
});

test( 'zlartg: general complex case f=(1+2i), g=(3+4i)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlartg_general'; } );
	var f = new Float64Array( [ 1.0, 2.0 ] );
	var g = new Float64Array( [ 3.0, 4.0 ] );
	var out = new Float64Array( 5 );
	base( f, g, out );
	assertClose( out[ 0 ], tc.c, 'zlartg_general c' );
	assertClose( out[ 1 ], tc.s[ 0 ], 'zlartg_general s[0]' );
	assertClose( out[ 2 ], tc.s[ 1 ], 'zlartg_general s[1]' );
	assertClose( out[ 3 ], tc.r[ 0 ], 'zlartg_general r[0]' );
	assertClose( out[ 4 ], tc.r[ 1 ], 'zlartg_general r[1]' );
});

test( 'zlartg: purely imaginary f=(0+2i), g=(0+3i)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlartg_pure_imag'; } );
	var f = new Float64Array( [ 0.0, 2.0 ] );
	var g = new Float64Array( [ 0.0, 3.0 ] );
	var out = new Float64Array( 5 );
	base( f, g, out );
	assertClose( out[ 0 ], tc.c, 'zlartg_pure_imag c' );
	assertClose( out[ 1 ], tc.s[ 0 ], 'zlartg_pure_imag s[0]' );
	assertClose( out[ 2 ], tc.s[ 1 ], 'zlartg_pure_imag s[1]' );
	assertClose( out[ 3 ], tc.r[ 0 ], 'zlartg_pure_imag r[0]' );
	assertClose( out[ 4 ], tc.r[ 1 ], 'zlartg_pure_imag r[1]' );
});

test( 'zlartg: f=0, g purely imaginary (0+5i)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlartg_f_zero_g_imag'; } );
	var f = new Float64Array( [ 0.0, 0.0 ] );
	var g = new Float64Array( [ 0.0, 5.0 ] );
	var out = new Float64Array( 5 );
	base( f, g, out );
	assertClose( out[ 0 ], tc.c, 'zlartg_f_zero_g_imag c' );
	assertClose( out[ 1 ], tc.s[ 0 ], 'zlartg_f_zero_g_imag s[0]' );
	assertClose( out[ 2 ], tc.s[ 1 ], 'zlartg_f_zero_g_imag s[1]' );
	assertClose( out[ 3 ], tc.r[ 0 ], 'zlartg_f_zero_g_imag r[0]' );
	assertClose( out[ 4 ], tc.r[ 1 ], 'zlartg_f_zero_g_imag r[1]' );
});

test( 'zlartg: f=0, g purely real (5+0i)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlartg_f_zero_g_real'; } );
	var f = new Float64Array( [ 0.0, 0.0 ] );
	var g = new Float64Array( [ 5.0, 0.0 ] );
	var out = new Float64Array( 5 );
	base( f, g, out );
	assertClose( out[ 0 ], tc.c, 'zlartg_f_zero_g_real c' );
	assertClose( out[ 1 ], tc.s[ 0 ], 'zlartg_f_zero_g_real s[0]' );
	assertClose( out[ 2 ], tc.s[ 1 ], 'zlartg_f_zero_g_real s[1]' );
	assertClose( out[ 3 ], tc.r[ 0 ], 'zlartg_f_zero_g_real r[0]' );
	assertClose( out[ 4 ], tc.r[ 1 ], 'zlartg_f_zero_g_real r[1]' );
});

test( 'zlartg: very large f (scaled algorithm)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlartg_large_f'; } );
	var f = new Float64Array( [ 1e200, 1e200 ] );
	var g = new Float64Array( [ 1.0, 1.0 ] );
	var out = new Float64Array( 5 );
	base( f, g, out );
	assertClose( out[ 0 ], tc.c, 'zlartg_large_f c' );
	assertClose( out[ 1 ], tc.s[ 0 ], 'zlartg_large_f s[0]' );
	assertClose( out[ 2 ], tc.s[ 1 ], 'zlartg_large_f s[1]' );
	assertClose( out[ 3 ], tc.r[ 0 ], 'zlartg_large_f r[0]' );
	assertClose( out[ 4 ], tc.r[ 1 ], 'zlartg_large_f r[1]' );
});

test( 'zlartg: very small f (scaled algorithm, f not well-scaled)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlartg_small_f'; } );
	var f = new Float64Array( [ 1e-200, 1e-200 ] );
	var g = new Float64Array( [ 1.0, 1.0 ] );
	var out = new Float64Array( 5 );
	base( f, g, out );
	assertClose( out[ 0 ], tc.c, 'zlartg_small_f c' );
	assertClose( out[ 1 ], tc.s[ 0 ], 'zlartg_small_f s[0]' );
	assertClose( out[ 2 ], tc.s[ 1 ], 'zlartg_small_f s[1]' );
	assertClose( out[ 3 ], tc.r[ 0 ], 'zlartg_small_f r[0]' );
	assertClose( out[ 4 ], tc.r[ 1 ], 'zlartg_small_f r[1]' );
});

test( 'zlartg: both very large (scaled algorithm, same scale)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlartg_both_large'; } );
	var f = new Float64Array( [ 3e200, 4e200 ] );
	var g = new Float64Array( [ 1e200, 2e200 ] );
	var out = new Float64Array( 5 );
	base( f, g, out );
	assertClose( out[ 0 ], tc.c, 'zlartg_both_large c' );
	assertClose( out[ 1 ], tc.s[ 0 ], 'zlartg_both_large s[0]' );
	assertClose( out[ 2 ], tc.s[ 1 ], 'zlartg_both_large s[1]' );
	assertClose( out[ 3 ], tc.r[ 0 ], 'zlartg_both_large r[0]' );
	assertClose( out[ 4 ], tc.r[ 1 ], 'zlartg_both_large r[1]' );
});

test( 'zlartg: normal f, very large g (scaled algorithm)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlartg_large_g'; } );
	var f = new Float64Array( [ 1.0, 1.0 ] );
	var g = new Float64Array( [ 1e200, 1e200 ] );
	var out = new Float64Array( 5 );
	base( f, g, out );
	assertClose( out[ 0 ], tc.c, 'zlartg_large_g c' );
	assertClose( out[ 1 ], tc.s[ 0 ], 'zlartg_large_g s[0]' );
	assertClose( out[ 2 ], tc.s[ 1 ], 'zlartg_large_g s[1]' );
	assertClose( out[ 3 ], tc.r[ 0 ], 'zlartg_large_g r[0]' );
	assertClose( out[ 4 ], tc.r[ 1 ], 'zlartg_large_g r[1]' );
});

test( 'zlartg: f=0, very large g (scaled f=0 path)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlartg_f_zero_g_large'; } );
	var f = new Float64Array( [ 0.0, 0.0 ] );
	var g = new Float64Array( [ 1e200, 2e200 ] );
	var out = new Float64Array( 5 );
	base( f, g, out );
	assertClose( out[ 0 ], tc.c, 'zlartg_f_zero_g_large c' );
	assertClose( out[ 1 ], tc.s[ 0 ], 'zlartg_f_zero_g_large s[0]' );
	assertClose( out[ 2 ], tc.s[ 1 ], 'zlartg_f_zero_g_large s[1]' );
	assertClose( out[ 3 ], tc.r[ 0 ], 'zlartg_f_zero_g_large r[0]' );
	assertClose( out[ 4 ], tc.r[ 1 ], 'zlartg_f_zero_g_large r[1]' );
});

test( 'zlartg: both very small (scaled algorithm)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlartg_both_small'; } );
	var f = new Float64Array( [ 3e-200, 4e-200 ] );
	var g = new Float64Array( [ 1e-200, 2e-200 ] );
	var out = new Float64Array( 5 );
	base( f, g, out );
	assertClose( out[ 0 ], tc.c, 'zlartg_both_small c' );
	assertClose( out[ 1 ], tc.s[ 0 ], 'zlartg_both_small s[0]' );
	assertClose( out[ 2 ], tc.s[ 1 ], 'zlartg_both_small s[1]' );
	assertClose( out[ 3 ], tc.r[ 0 ], 'zlartg_both_small r[0]' );
	assertClose( out[ 4 ], tc.r[ 1 ], 'zlartg_both_small r[1]' );
});

test( 'zlartg: large f, very small g', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlartg_large_f_small_g'; } );
	var f = new Float64Array( [ 1e200, 0.0 ] );
	var g = new Float64Array( [ 1e-200, 0.0 ] );
	var out = new Float64Array( 5 );
	base( f, g, out );
	assertClose( out[ 0 ], tc.c, 'zlartg_large_f_small_g c' );
	assertClose( out[ 1 ], tc.s[ 0 ], 'zlartg_large_f_small_g s[0]' );
	assertClose( out[ 2 ], tc.s[ 1 ], 'zlartg_large_f_small_g s[1]' );
	assertClose( out[ 3 ], tc.r[ 0 ], 'zlartg_large_f_small_g r[0]' );
	assertClose( out[ 4 ], tc.r[ 1 ], 'zlartg_large_f_small_g r[1]' );
});

test( 'zlartg: tiny f, g=0', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlartg_tiny_f_g_zero'; } );
	var f = new Float64Array( [ 1e-300, 1e-300 ] );
	var g = new Float64Array( [ 0.0, 0.0 ] );
	var out = new Float64Array( 5 );
	base( f, g, out );
	assertClose( out[ 0 ], tc.c, 'zlartg_tiny_f_g_zero c' );
	assertClose( out[ 1 ], tc.s[ 0 ], 'zlartg_tiny_f_g_zero s[0]' );
	assertClose( out[ 2 ], tc.s[ 1 ], 'zlartg_tiny_f_g_zero s[1]' );
	assertClose( out[ 3 ], tc.r[ 0 ], 'zlartg_tiny_f_g_zero r[0]' );
	assertClose( out[ 4 ], tc.r[ 1 ], 'zlartg_tiny_f_g_zero r[1]' );
});

test( 'zlartg: f=0, very small g (scaled f=0 path)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlartg_f_zero_g_small'; } );
	var f = new Float64Array( [ 0.0, 0.0 ] );
	var g = new Float64Array( [ 1e-200, 2e-200 ] );
	var out = new Float64Array( 5 );
	base( f, g, out );
	assertClose( out[ 0 ], tc.c, 'zlartg_f_zero_g_small c' );
	assertClose( out[ 1 ], tc.s[ 0 ], 'zlartg_f_zero_g_small s[0]' );
	assertClose( out[ 2 ], tc.s[ 1 ], 'zlartg_f_zero_g_small s[1]' );
	assertClose( out[ 3 ], tc.r[ 0 ], 'zlartg_f_zero_g_small r[0]' );
	assertClose( out[ 4 ], tc.r[ 1 ], 'zlartg_f_zero_g_small r[1]' );
});

test( 'zlartg: unscaled, f2 < h2*SAFMIN (tiny f relative to g)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlartg_unscaled_tiny_f'; } );
	var f = new Float64Array( [ 1e-54, 0.0 ] );
	var g = new Float64Array( [ 1e100, 0.0 ] );
	var out = new Float64Array( 5 );
	base( f, g, out );
	assertClose( out[ 0 ], tc.c, 'zlartg_unscaled_tiny_f c' );
	assertClose( out[ 1 ], tc.s[ 0 ], 'zlartg_unscaled_tiny_f s[0]' );
	assertClose( out[ 2 ], tc.s[ 1 ], 'zlartg_unscaled_tiny_f s[1]' );
	assertClose( out[ 3 ], tc.r[ 0 ], 'zlartg_unscaled_tiny_f r[0]' );
	assertClose( out[ 4 ], tc.r[ 1 ], 'zlartg_unscaled_tiny_f r[1]' );
});

test( 'zlartg: unscaled, f2 small (else branch of f2>rtmin)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlartg_unscaled_f2_small'; } );
	var f = new Float64Array( [ 1e-100, 0.0 ] );
	var g = new Float64Array( [ 1.0, 0.0 ] );
	var out = new Float64Array( 5 );
	base( f, g, out );
	assertClose( out[ 0 ], tc.c, 'zlartg_unscaled_f2_small c' );
	assertClose( out[ 1 ], tc.s[ 0 ], 'zlartg_unscaled_f2_small s[0]' );
	assertClose( out[ 2 ], tc.s[ 1 ], 'zlartg_unscaled_f2_small s[1]' );
	assertClose( out[ 3 ], tc.r[ 0 ], 'zlartg_unscaled_f2_small r[0]' );
	assertClose( out[ 4 ], tc.r[ 1 ], 'zlartg_unscaled_f2_small r[1]' );
});

test( 'zlartg: scaled, f2 < h2*SAFMIN (tiny f in scaled path)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlartg_scaled_tiny_f'; } );
	var f = new Float64Array( [ 1e-100, 0.0 ] );
	var g = new Float64Array( [ 1e200, 1e200 ] );
	var out = new Float64Array( 5 );
	base( f, g, out );
	assertClose( out[ 0 ], tc.c, 'zlartg_scaled_tiny_f c' );
	assertClose( out[ 1 ], tc.s[ 0 ], 'zlartg_scaled_tiny_f s[0]' );
	assertClose( out[ 2 ], tc.s[ 1 ], 'zlartg_scaled_tiny_f s[1]' );
	assertClose( out[ 3 ], tc.r[ 0 ], 'zlartg_scaled_tiny_f r[0]' );
	assertClose( out[ 4 ], tc.r[ 1 ], 'zlartg_scaled_tiny_f r[1]' );
});

test( 'zlartg: scaled, large both, h2 >= rtmax (else branch in scaled)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlartg_scaled_large_both'; } );
	var f = new Float64Array( [ 5e152, 5e152 ] );
	var g = new Float64Array( [ 5e152, 5e152 ] );
	var out = new Float64Array( 5 );
	base( f, g, out );
	assertClose( out[ 0 ], tc.c, 'zlartg_scaled_large_both c' );
	assertClose( out[ 1 ], tc.s[ 0 ], 'zlartg_scaled_large_both s[0]' );
	assertClose( out[ 2 ], tc.s[ 1 ], 'zlartg_scaled_large_both s[1]' );
	assertClose( out[ 3 ], tc.r[ 0 ], 'zlartg_scaled_large_both r[0]' );
	assertClose( out[ 4 ], tc.r[ 1 ], 'zlartg_scaled_large_both r[1]' );
});

test( 'zlartg: scaled, same scaling, f2 < h2*SAFMIN', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlartg_scaled_f2_lt_safmin'; } );
	var f = new Float64Array( [ 2e46, 0.0 ] );
	var g = new Float64Array( [ 1e200, 1e200 ] );
	var out = new Float64Array( 5 );
	base( f, g, out );
	assertClose( out[ 0 ], tc.c, 'zlartg_scaled_f2_lt_safmin c' );
	assertClose( out[ 1 ], tc.s[ 0 ], 'zlartg_scaled_f2_lt_safmin s[0]' );
	assertClose( out[ 2 ], tc.s[ 1 ], 'zlartg_scaled_f2_lt_safmin s[1]' );
	assertClose( out[ 3 ], tc.r[ 0 ], 'zlartg_scaled_f2_lt_safmin r[0]' );
	assertClose( out[ 4 ], tc.r[ 1 ], 'zlartg_scaled_f2_lt_safmin r[1]' );
});
