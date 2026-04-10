/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

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

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var dlarfgp = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dlarfgp.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line max-len
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 ); // eslint-disable-line max-len
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual ); // eslint-disable-line max-len
}

function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' ); // eslint-disable-line max-len
	}
}


// TESTS //

test( 'dlarfgp is a function', function t() {
	assert.strictEqual( typeof dlarfgp, 'function' );
} );

test( 'dlarfgp: basic', function t() {
	var tc = findCase( 'basic' );
	var alpha = new Float64Array( [ 3.0 ] );
	var tau = new Float64Array( 1 );
	var x = new Float64Array( [ 4.0, 0.0, 0.0 ] );

	dlarfgp( 4, alpha, 0, x, 1, 0, tau, 0 );

	assertClose( alpha[ 0 ], tc.alpha, 1e-14, 'alpha' );
	assertClose( tau[ 0 ], tc.tau, 1e-14, 'tau' );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
	assert.ok( alpha[ 0 ] >= 0.0, 'beta non-negative' );
} );

test( 'dlarfgp: alpha=0', function t() {
	var tc = findCase( 'alpha_zero' );
	var alpha = new Float64Array( [ 0.0 ] );
	var tau = new Float64Array( 1 );
	var x = new Float64Array( [ 3.0, 4.0 ] );

	dlarfgp( 3, alpha, 0, x, 1, 0, tau, 0 );

	assertClose( alpha[ 0 ], tc.alpha, 1e-14, 'alpha' );
	assertClose( tau[ 0 ], tc.tau, 1e-14, 'tau' );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
	assert.ok( alpha[ 0 ] >= 0.0, 'beta non-negative' );
} );

test( 'dlarfgp: n=1 (tau=0)', function t() {
	var tc = findCase( 'n_one' );
	var alpha = new Float64Array( [ 5.0 ] );
	var tau = new Float64Array( 1 );
	var x = new Float64Array( 1 );

	dlarfgp( 1, alpha, 0, x, 1, 0, tau, 0 );

	assertClose( tau[ 0 ], tc.tau, 1e-14, 'tau' );
	assertClose( alpha[ 0 ], tc.alpha, 1e-14, 'alpha' );
} );

test( 'dlarfgp: x all zero, alpha positive', function t() {
	var tc = findCase( 'x_zero_pos_alpha' );
	var alpha = new Float64Array( [ 5.0 ] );
	var tau = new Float64Array( 1 );
	var x = new Float64Array( [ 0.0, 0.0 ] );

	dlarfgp( 3, alpha, 0, x, 1, 0, tau, 0 );

	assertClose( alpha[ 0 ], tc.alpha, 1e-14, 'alpha' );
	assertClose( tau[ 0 ], tc.tau, 1e-14, 'tau' );
} );

test( 'dlarfgp: x all zero, alpha negative', function t() {
	var tc = findCase( 'x_zero_neg_alpha' );
	var alpha = new Float64Array( [ -5.0 ] );
	var tau = new Float64Array( 1 );
	var x = new Float64Array( [ 0.0, 0.0 ] );

	dlarfgp( 3, alpha, 0, x, 1, 0, tau, 0 );

	assertClose( alpha[ 0 ], tc.alpha, 1e-14, 'alpha' );
	assertClose( tau[ 0 ], tc.tau, 1e-14, 'tau' );
	assert.strictEqual( tau[ 0 ], 2.0, 'tau = 2 for negative alpha + zero x' );
} );

test( 'dlarfgp: negative alpha', function t() {
	var tc = findCase( 'negative_alpha' );
	var alpha = new Float64Array( [ -3.0 ] );
	var tau = new Float64Array( 1 );
	var x = new Float64Array( [ 4.0 ] );

	dlarfgp( 2, alpha, 0, x, 1, 0, tau, 0 );

	assertClose( alpha[ 0 ], tc.alpha, 1e-14, 'alpha' );
	assertClose( tau[ 0 ], tc.tau, 1e-14, 'tau' );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
	assert.ok( alpha[ 0 ] >= 0.0, 'beta non-negative' );
} );

test( 'dlarfgp: n=0 (tau=0)', function t() {
	var tc = findCase( 'n_zero' );
	var alpha = new Float64Array( [ 5.0 ] );
	var tau = new Float64Array( [ 99.0 ] );
	var x = new Float64Array( 1 );

	dlarfgp( 0, alpha, 0, x, 1, 0, tau, 0 );

	assertClose( tau[ 0 ], tc.tau, 1e-14, 'tau' );
} );

test( 'dlarfgp: non-unit stride', function t() {
	var tc = findCase( 'stride2' );
	var alpha = new Float64Array( [ 2.0 ] );
	var tau = new Float64Array( 1 );
	var x = new Float64Array( [ 1.0, 99.0, 2.0, 99.0, 2.0 ] );

	dlarfgp( 4, alpha, 0, x, 2, 0, tau, 0 );

	assertClose( alpha[ 0 ], tc.alpha, 1e-14, 'alpha' );
	assertClose( tau[ 0 ], tc.tau, 1e-14, 'tau' );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
	assert.strictEqual( x[ 1 ], 99.0 );
	assert.strictEqual( x[ 3 ], 99.0 );
	assert.ok( alpha[ 0 ] >= 0.0, 'beta non-negative' );
} );

test( 'dlarfgp: larger n', function t() {
	var tc = findCase( 'larger_n' );
	var alpha = new Float64Array( [ 1.5 ] );
	var tau = new Float64Array( 1 );
	var x = new Float64Array( [ 0.5, -0.25, 0.75, -0.125, 0.0625 ] );

	dlarfgp( 6, alpha, 0, x, 1, 0, tau, 0 );

	assertClose( alpha[ 0 ], tc.alpha, 1e-14, 'alpha' );
	assertClose( tau[ 0 ], tc.tau, 1e-14, 'tau' );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
	assert.ok( alpha[ 0 ] >= 0.0, 'beta non-negative' );
} );

test( 'dlarfgp: tiny inputs (smlnum scaling loop)', function t() {
	var alpha = new Float64Array( [ 1e-300 ] );
	var x = new Float64Array( [ 1e-300, 1e-300 ] );
	var tau = new Float64Array( 1 );

	dlarfgp( 3, alpha, 0, x, 1, 0, tau, 0 );

	assert.ok( alpha[ 0 ] >= 0.0, 'beta non-negative' );
	assert.ok( tau[ 0 ] >= 0.0 && tau[ 0 ] <= 2.0, 'tau in [0, 2]' );
} );

test( 'dlarfgp: tiny negative alpha', function t() {
	var alpha = new Float64Array( [ -2e-300 ] );
	var x = new Float64Array( [ 1e-300 ] );
	var tau = new Float64Array( 1 );

	dlarfgp( 2, alpha, 0, x, 1, 0, tau, 0 );

	assert.ok( alpha[ 0 ] >= 0.0, 'beta non-negative' );
	assert.ok( tau[ 0 ] >= 0.0 && tau[ 0 ] <= 2.0, 'tau in [0, 2]' );
} );

test( 'dlarfgp: offset into alpha and tau', function t() {
	var alpha = new Float64Array( [ 99.0, 3.0, 99.0 ] );
	var x = new Float64Array( [ 4.0, 0.0, 0.0 ] );
	var tau = new Float64Array( [ 99.0, 0.0, 99.0 ] );

	dlarfgp( 4, alpha, 1, x, 1, 0, tau, 1 );

	assert.strictEqual( alpha[ 0 ], 99.0 );
	assert.strictEqual( alpha[ 2 ], 99.0 );
	assert.strictEqual( tau[ 0 ], 99.0 );
	assert.strictEqual( tau[ 2 ], 99.0 );
	assert.ok( alpha[ 1 ] >= 0.0, 'beta non-negative' );
} );

test( 'dlarfgp: offset into x array', function t() {
	var alpha = new Float64Array( [ 3.0 ] );
	var x = new Float64Array( [ 99.0, 99.0, 4.0, 0.0, 0.0 ] );
	var tau = new Float64Array( 1 );

	dlarfgp( 4, alpha, 0, x, 1, 2, tau, 0 );

	assert.strictEqual( x[ 0 ], 99.0 );
	assert.strictEqual( x[ 1 ], 99.0 );
	assert.ok( alpha[ 0 ] >= 0.0, 'beta non-negative' );
} );

test( 'dlarfgp: reflector property H*u = beta*e1 with beta>=0', function t() {
	var alpha = new Float64Array( [ 3.0 ] );
	var x = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
	var tau = new Float64Array( 1 );
	var u;
	var v;
	var beta;
	var dot;
	var hi;
	var i;

	dlarfgp( 5, alpha, 0, x, 1, 0, tau, 0 );

	beta = alpha[ 0 ];
	assert.ok( beta >= 0.0, 'beta non-negative' );

	v = new Float64Array( [ 1.0, x[ 0 ], x[ 1 ], x[ 2 ], x[ 3 ] ] );
	u = new Float64Array( [ 3.0, 1.0, 2.0, 3.0, 4.0 ] );

	dot = 0.0;
	for ( i = 0; i < 5; i++ ) {
		dot += v[ i ] * u[ i ];
	}
	for ( i = 0; i < 5; i++ ) {
		hi = u[ i ] - ( tau[ 0 ] * v[ i ] * dot );
		if ( i === 0 ) {
			assertClose( hi, beta, 1e-14, 'H*u[0] = beta' );
		} else {
			assertClose( hi, 0.0, 1e-14, 'H*u[' + i + '] = 0' );
		}
	}
} );
