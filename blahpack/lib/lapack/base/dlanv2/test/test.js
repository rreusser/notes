/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';


// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var dlanv2 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dlanv2.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
var fixture = lines.map( function parse( line ) {
	return JSON.parse( line );
} );


// FUNCTIONS //

/**
* Returns a test case from the fixture data.
*
* @private
* @param {string} name - test case name
* @returns {*} result
*/
function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name;
	} );
}

/**
* Asserts that two numbers are approximately equal.
*
* @private
* @param {*} actual - actual value
* @param {*} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 ); // eslint-disable-line max-len
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual + ' (relErr=' + relErr + ')' ); // eslint-disable-line max-len
}

/**
* CheckResult.
*
* @private
* @param {*} result - result
* @param {*} tc - tc
* @param {number} tol - tolerance
*/
function checkResult( result, tc, tol ) {
	assertClose( result.a, tc.a, tol, 'a' );
	assertClose( result.b, tc.b, tol, 'b' );
	assertClose( result.c, tc.c, tol, 'c' );
	assertClose( result.d, tc.d, tol, 'd' );
	assertClose( result.rt1r, tc.rt1r, tol, 'rt1r' );
	assertClose( result.rt1i, tc.rt1i, tol, 'rt1i' );
	assertClose( result.rt2r, tc.rt2r, tol, 'rt2r' );
	assertClose( result.rt2i, tc.rt2i, tol, 'rt2i' );
	assertClose( result.cs, tc.cs, tol, 'cs' );
	assertClose( result.sn, tc.sn, tol, 'sn' );
}

// Map test name -> input (a, b, c, d)
var inputs = {
	'c_zero': [ 1.0, 2.0, 0.0, 3.0 ],
	'b_zero': [ 3.0, 0.0, 4.0, 1.0 ],
	'a_eq_d_diff_sign': [ 2.0, 3.0, -1.0, 2.0 ],
	'real_eigenvalues': [ 4.0, 1.0, 2.0, 3.0 ],
	'complex_eigenvalues': [ 1.0, -5.0, 1.0, 1.0 ],
	'zero_matrix': [ 0.0, 0.0, 0.0, 0.0 ],
	'identity': [ 1.0, 0.0, 0.0, 1.0 ],
	'diagonal': [ 5.0, 0.0, 0.0, 2.0 ],
	'real_negative': [ -1.0, 3.0, 2.0, 4.0 ],
	'complex_equal_diag': [ 3.0, -4.0, 2.0, 3.0 ],
	'near_equal': [ 1.0, 1.0e-15, 1.0e-15, 1.0 ],
	'large_values': [ 1.0e150, 2.0e150, 3.0e150, 4.0e150 ],
	'same_sign_bc': [ 2.0, 3.0, 3.0, 2.0 ],
	'tiny_bc': [ 2.0, 1.0e-16, 1.0e-16, 2.0 ]
};


// TESTS //

test( 'dlanv2: c_zero (C=0, already Schur form)', function t() {
	var result;
	var inp;
	var tc;

	inp = inputs[ 'c_zero' ];
	tc = findCase( 'c_zero' );
	result = dlanv2( inp[0], inp[1], inp[2], inp[3] );
	checkResult( result, tc, 1e-14 );
});

test( 'dlanv2: b_zero (B=0, swap rows and columns)', function t() {
	var result;
	var inp;
	var tc;

	inp = inputs[ 'b_zero' ];
	tc = findCase( 'b_zero' );
	result = dlanv2( inp[0], inp[1], inp[2], inp[3] );
	checkResult( result, tc, 1e-14 );
});

test( 'dlanv2: a_eq_d_diff_sign (A=D, sign(B)!=sign(C))', function t() {
	var result;
	var inp;
	var tc;

	inp = inputs[ 'a_eq_d_diff_sign' ];
	tc = findCase( 'a_eq_d_diff_sign' );
	result = dlanv2( inp[0], inp[1], inp[2], inp[3] );
	checkResult( result, tc, 1e-14 );
});

test( 'dlanv2: real_eigenvalues (Z >= MULTPL*EPS)', function t() {
	var result;
	var inp;
	var tc;

	inp = inputs[ 'real_eigenvalues' ];
	tc = findCase( 'real_eigenvalues' );
	result = dlanv2( inp[0], inp[1], inp[2], inp[3] );
	checkResult( result, tc, 1e-14 );
});

test( 'dlanv2: complex_eigenvalues', function t() {
	var result;
	var inp;
	var tc;

	inp = inputs[ 'complex_eigenvalues' ];
	tc = findCase( 'complex_eigenvalues' );
	result = dlanv2( inp[0], inp[1], inp[2], inp[3] );
	checkResult( result, tc, 1e-14 );
});

test( 'dlanv2: zero_matrix', function t() {
	var result;
	var inp;
	var tc;

	inp = inputs[ 'zero_matrix' ];
	tc = findCase( 'zero_matrix' );
	result = dlanv2( inp[0], inp[1], inp[2], inp[3] );
	checkResult( result, tc, 1e-14 );
});

test( 'dlanv2: identity', function t() {
	var result;
	var inp;
	var tc;

	inp = inputs[ 'identity' ];
	tc = findCase( 'identity' );
	result = dlanv2( inp[0], inp[1], inp[2], inp[3] );
	checkResult( result, tc, 1e-14 );
});

test( 'dlanv2: diagonal', function t() {
	var result;
	var inp;
	var tc;

	inp = inputs[ 'diagonal' ];
	tc = findCase( 'diagonal' );
	result = dlanv2( inp[0], inp[1], inp[2], inp[3] );
	checkResult( result, tc, 1e-14 );
});

test( 'dlanv2: real_negative', function t() {
	var result;
	var inp;
	var tc;

	inp = inputs[ 'real_negative' ];
	tc = findCase( 'real_negative' );
	result = dlanv2( inp[0], inp[1], inp[2], inp[3] );
	checkResult( result, tc, 1e-14 );
});

test( 'dlanv2: complex_equal_diag', function t() {
	var result;
	var inp;
	var tc;

	inp = inputs[ 'complex_equal_diag' ];
	tc = findCase( 'complex_equal_diag' );
	result = dlanv2( inp[0], inp[1], inp[2], inp[3] );
	checkResult( result, tc, 1e-14 );
});

test( 'dlanv2: near_equal (small Z, complex path)', function t() {
	var result;
	var inp;
	var tc;

	inp = inputs[ 'near_equal' ];
	tc = findCase( 'near_equal' );
	result = dlanv2( inp[0], inp[1], inp[2], inp[3] );
	checkResult( result, tc, 1e-14 );
});

test( 'dlanv2: large_values', function t() {
	var result;
	var inp;
	var tc;

	inp = inputs[ 'large_values' ];
	tc = findCase( 'large_values' );
	result = dlanv2( inp[0], inp[1], inp[2], inp[3] );
	checkResult( result, tc, 1e-14 );
});

test( 'dlanv2: same_sign_bc (sign(B)==sign(C), real reduction)', function t() {
	var result;
	var inp;
	var tc;

	inp = inputs[ 'same_sign_bc' ];
	tc = findCase( 'same_sign_bc' );
	result = dlanv2( inp[0], inp[1], inp[2], inp[3] );
	checkResult( result, tc, 1e-14 );
});

test( 'dlanv2: tiny_bc (near-zero B,C)', function t() {
	var result;
	var inp;
	var tc;

	inp = inputs[ 'tiny_bc' ];
	tc = findCase( 'tiny_bc' );
	result = dlanv2( inp[0], inp[1], inp[2], inp[3] );
	checkResult( result, tc, 1e-14 );
});

// Verify Schur factorization property: [CS -SN; SN CS]' * [A B; C D] * [CS -SN; SN CS] = Schur form // eslint-disable-line max-len
test( 'dlanv2: verify Schur factorization property', function t() {
	var names = Object.keys( inputs );
	var inp;
	var a11;
	var a12;
	var a21;
	var a22;
	var i;
	var r;

	for ( i = 0; i < names.length; i++ ) {
		inp = inputs[ names[i] ];
		r = dlanv2( inp[0], inp[1], inp[2], inp[3] );

		// From Fortran doc:

		// [A B] = [CS -SN] [AA BB] [ CS SN]

		// [C D]   [SN  CS] [CC DD] [-SN CS]

		// So original = Q * Schur * Q^T where Q = [CS -SN; SN CS]

		// Q^T = [CS SN; -SN CS]

		// First compute T = Schur * Q^T:

		// t11 = r.a*r.cs + r.b*(-r.sn)

		// t12 = r.a*r.sn + r.b*r.cs

		// t21 = r.c*r.cs + r.d*(-r.sn)

		// t22 = r.c*r.sn + r.d*r.cs

		// Then original = Q * T:

		// a11 = cs*t11 + (-sn)*t21, etc.
		var t11 = r.a * r.cs - r.b * r.sn;
		var t12 = r.a * r.sn + r.b * r.cs;
		var t21 = r.c * r.cs - r.d * r.sn;
		var t22 = r.c * r.sn + r.d * r.cs;
		a11 = r.cs * t11 - r.sn * t21;
		a12 = r.cs * t12 - r.sn * t22;
		a21 = r.sn * t11 + r.cs * t21;
		a22 = r.sn * t12 + r.cs * t22;

		assertClose( a11, inp[0], 1e-12, names[i] + ' reconstruct a' );
		assertClose( a12, inp[1], 1e-12, names[i] + ' reconstruct b' );
		assertClose( a21, inp[2], 1e-12, names[i] + ' reconstruct c' );
		assertClose( a22, inp[3], 1e-12, names[i] + ' reconstruct d' );
	}
});
