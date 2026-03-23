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
var Complex128Array = require( '@stdlib/array/complex128' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var zlange = require( './../lib' );
var base = require( './../lib/base.js' );

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zlange.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );

// HELPERS //

function assertClose( actual, expected, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= 1e-14, msg + ': expected ' + expected + ', got ' + actual );
}

// TESTS //

test( 'zlange: main export is a function', function t() {
	assert.strictEqual( typeof zlange, 'function' );
});

test( 'zlange: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof zlange.ndarray, 'function' );
});

test( 'zlange: max norm (M)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlange_max'; } );
	// A col-major 2x2: [(1+2i), (3+4i), (5+6i), (7+8i)]
	var A = new Complex128Array( [ 1, 2, 3, 4, 5, 6, 7, 8 ] );
	var work = new Float64Array( 10 );
	var result = base( 'max', 2, 2, A, 1, 2, 0, work, 1, 0 );
	assertClose( result, tc.result, 'zlange_max' );
});

test( 'zlange: one norm (1)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlange_one'; } );
	var A = new Complex128Array( [ 1, 2, 3, 4, 5, 6, 7, 8 ] );
	var work = new Float64Array( 10 );
	var result = base( 'one-norm', 2, 2, A, 1, 2, 0, work, 1, 0 );
	assertClose( result, tc.result, 'zlange_one' );
});

test( 'zlange: infinity norm (I)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlange_inf'; } );
	var A = new Complex128Array( [ 1, 2, 3, 4, 5, 6, 7, 8 ] );
	var work = new Float64Array( 10 );
	var result = base( 'inf-norm', 2, 2, A, 1, 2, 0, work, 1, 0 );
	assertClose( result, tc.result, 'zlange_inf' );
});

test( 'zlange: Frobenius norm (F)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlange_frob'; } );
	var A = new Complex128Array( [ 1, 2, 3, 4, 5, 6, 7, 8 ] );
	var work = new Float64Array( 10 );
	var result = base( 'frobenius', 2, 2, A, 1, 2, 0, work, 1, 0 );
	assertClose( result, tc.result, 'zlange_frob' );
});

test( 'zlange: M=0 quick return', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlange_m_zero'; } );
	var A = new Complex128Array( 1 );
	var work = new Float64Array( 1 );
	var result = base( 'max', 0, 2, A, 1, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 'zlange_m_zero' );
});

test( 'zlange: N=0 quick return', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlange_n_zero'; } );
	var A = new Complex128Array( 1 );
	var work = new Float64Array( 1 );
	var result = base( 'max', 2, 0, A, 1, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 'zlange_n_zero' );
});

test( 'zlange: 1x1 matrix Frobenius', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlange_1x1'; } );
	// A = [(3,4)]
	var A = new Complex128Array( [ 3, 4 ] );
	var work = new Float64Array( 1 );
	var result = base( 'frobenius', 1, 1, A, 1, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 'zlange_1x1' );
});

test( 'zlange: unrecognized norm returns 0', function t() {
	var A = new Complex128Array( [ 1, 2, 3, 4 ] );
	var work = new Float64Array( 2 );
	var result = base( 'X', 1, 2, A, 1, 1, 0, work, 1, 0 );
	assert.strictEqual( result, 0.0 );
});

test( 'zlange: NaN in matrix propagates through max norm', function t() {
	var A = new Complex128Array( [ NaN, 0, 1, 0 ] );
	var work = new Float64Array( 2 );
	var result = base( 'max', 1, 2, A, 1, 1, 0, work, 1, 0 );
	assert.ok( result !== result, 'expected NaN' );
});

test( 'zlange: NaN in matrix propagates through one norm', function t() {
	var A = new Complex128Array( [ NaN, 0, 1, 0 ] );
	var work = new Float64Array( 2 );
	var result = base( 'one-norm', 1, 2, A, 1, 1, 0, work, 1, 0 );
	assert.ok( result !== result, 'expected NaN' );
});

test( 'zlange: NaN in matrix propagates through inf norm', function t() {
	var A = new Complex128Array( [ NaN, 0, 1, 0 ] );
	var work = new Float64Array( 2 );
	var result = base( 'inf-norm', 1, 2, A, 1, 1, 0, work, 1, 0 );
	assert.ok( result !== result, 'expected NaN' );
});
