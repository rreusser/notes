/* eslint-disable no-restricted-syntax, stdlib/first-unit-test, max-lines */

'use strict';

// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlagv2 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dlagv2.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line max-len, node/no-sync
var fixture = lines.map( function parse( line ) {
	return JSON.parse( line );
} );


// FUNCTIONS //

/**
* Finds a test case by name in the fixture data.
*
* @private
* @param {string} name - test case name
* @returns {Object} test case object
*/
function findCase( name ) {
	return fixture.find( function find( t ) {
		return t.name === name;
	} );
}

/**
* Converts a typed array to a plain array.
*
* @private
* @param {Float64Array} arr - typed array
* @returns {Array} plain array
*/
function toArray( arr ) {
	var out;
	var i;
	out = [];
	for ( i = 0; i < arr.length; i++ ) {
		out.push( arr[ i ] );
	}
	return out;
}

/**
* Asserts that two numbers are approximately equal.
*
* @private
* @param {number} actual - actual value
* @param {number} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - message
*/
function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 ); // eslint-disable-line max-len
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

/**
* Asserts that two arrays are approximately element-wise equal.
*
* @private
* @param {Float64Array} actual - actual array
* @param {Array} expected - expected array
* @param {number} tol - tolerance
* @param {string} msg - message
*/
function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}


// TESTS //

test( 'dlagv2 is a function', function t() {
	assert.equal( typeof dlagv2, 'function' );
});

test( 'dlagv2: already_upper', function t() {
	var alphar;
	var alphai;
	var result;
	var beta;
	var tc;
	var A;
	var B;

	alphar = new Float64Array( 2 );
	alphai = new Float64Array( 2 );
	beta = new Float64Array( 2 );
	tc = findCase( 'already_upper' );
	A = new Float64Array( [ 4.0, 0.0, 2.0, 3.0 ] );
	B = new Float64Array( [ 1.0, 0.0, 0.5, 2.0 ] );
	result = dlagv2( A, 1, 2, 0, B, 1, 2, 0, alphar, 1, 0, alphai, 1, 0, beta, 1, 0 ); // eslint-disable-line max-len
	assertArrayClose( toArray( A ), tc.A, 1e-14, 'A' );
	assertArrayClose( toArray( B ), tc.B, 1e-14, 'B' );
	assertArrayClose( toArray( alphar ), tc.ALPHAR, 1e-14, 'ALPHAR' );
	assertArrayClose( toArray( alphai ), tc.ALPHAI, 1e-14, 'ALPHAI' );
	assertArrayClose( toArray( beta ), tc.BETA, 1e-14, 'BETA' );
	assertClose( result.CSL, tc.CSL, 1e-14, 'CSL' );
	assertClose( result.SNL, tc.SNL, 1e-14, 'SNL' );
	assertClose( result.CSR, tc.CSR, 1e-14, 'CSR' );
	assertClose( result.SNR, tc.SNR, 1e-14, 'SNR' );
});

test( 'dlagv2: b11_zero', function t() {
	var alphar;
	var alphai;
	var result;
	var beta;
	var tc;
	var A;
	var B;

	alphar = new Float64Array( 2 );
	alphai = new Float64Array( 2 );
	beta = new Float64Array( 2 );
	tc = findCase( 'b11_zero' );
	A = new Float64Array( [ 3.0, 4.0, 1.0, 2.0 ] );
	B = new Float64Array( [ 0.0, 0.0, 1.0, 3.0 ] );
	result = dlagv2( A, 1, 2, 0, B, 1, 2, 0, alphar, 1, 0, alphai, 1, 0, beta, 1, 0 ); // eslint-disable-line max-len
	assertArrayClose( toArray( A ), tc.A, 1e-14, 'A' );
	assertArrayClose( toArray( B ), tc.B, 1e-14, 'B' );
	assertArrayClose( toArray( alphar ), tc.ALPHAR, 1e-14, 'ALPHAR' );
	assertArrayClose( toArray( alphai ), tc.ALPHAI, 1e-14, 'ALPHAI' );
	assertArrayClose( toArray( beta ), tc.BETA, 1e-14, 'BETA' );
	assertClose( result.CSL, tc.CSL, 1e-14, 'CSL' );
	assertClose( result.SNL, tc.SNL, 1e-14, 'SNL' );
	assertClose( result.CSR, tc.CSR, 1e-14, 'CSR' );
	assertClose( result.SNR, tc.SNR, 1e-14, 'SNR' );
});

test( 'dlagv2: b22_zero', function t() {
	var alphar;
	var alphai;
	var result;
	var beta;
	var tc;
	var A;
	var B;

	alphar = new Float64Array( 2 );
	alphai = new Float64Array( 2 );
	beta = new Float64Array( 2 );
	tc = findCase( 'b22_zero' );
	A = new Float64Array( [ 2.0, 3.0, 1.0, 5.0 ] );
	B = new Float64Array( [ 4.0, 0.0, 2.0, 0.0 ] );
	result = dlagv2( A, 1, 2, 0, B, 1, 2, 0, alphar, 1, 0, alphai, 1, 0, beta, 1, 0 ); // eslint-disable-line max-len
	assertArrayClose( toArray( A ), tc.A, 1e-14, 'A' );
	assertArrayClose( toArray( B ), tc.B, 1e-14, 'B' );
	assertArrayClose( toArray( alphar ), tc.ALPHAR, 1e-14, 'ALPHAR' );
	assertArrayClose( toArray( alphai ), tc.ALPHAI, 1e-14, 'ALPHAI' );
	assertArrayClose( toArray( beta ), tc.BETA, 1e-14, 'BETA' );
	assertClose( result.CSL, tc.CSL, 1e-14, 'CSL' );
	assertClose( result.SNL, tc.SNL, 1e-14, 'SNL' );
	assertClose( result.CSR, tc.CSR, 1e-14, 'CSR' );
	assertClose( result.SNR, tc.SNR, 1e-14, 'SNR' );
});

test( 'dlagv2: real_eigenvalues', function t() {
	var alphar;
	var alphai;
	var result;
	var beta;
	var tc;
	var A;
	var B;

	alphar = new Float64Array( 2 );
	alphai = new Float64Array( 2 );
	beta = new Float64Array( 2 );
	tc = findCase( 'real_eigenvalues' );
	A = new Float64Array( [ 4.0, 2.0, 1.0, 3.0 ] );
	B = new Float64Array( [ 2.0, 0.0, 1.0, 1.0 ] );
	result = dlagv2( A, 1, 2, 0, B, 1, 2, 0, alphar, 1, 0, alphai, 1, 0, beta, 1, 0 ); // eslint-disable-line max-len
	assertArrayClose( toArray( A ), tc.A, 1e-14, 'A' );
	assertArrayClose( toArray( B ), tc.B, 1e-14, 'B' );
	assertArrayClose( toArray( alphar ), tc.ALPHAR, 1e-14, 'ALPHAR' );
	assertArrayClose( toArray( alphai ), tc.ALPHAI, 1e-14, 'ALPHAI' );
	assertArrayClose( toArray( beta ), tc.BETA, 1e-14, 'BETA' );
	assertClose( result.CSL, tc.CSL, 1e-14, 'CSL' );
	assertClose( result.SNL, tc.SNL, 1e-14, 'SNL' );
	assertClose( result.CSR, tc.CSR, 1e-14, 'CSR' );
	assertClose( result.SNR, tc.SNR, 1e-14, 'SNR' );
});

test( 'dlagv2: complex_eigenvalues', function t() {
	var alphar;
	var alphai;
	var result;
	var beta;
	var tc;
	var A;
	var B;

	alphar = new Float64Array( 2 );
	alphai = new Float64Array( 2 );
	beta = new Float64Array( 2 );
	tc = findCase( 'complex_eigenvalues' );
	A = new Float64Array( [ 1.0, 3.0, -5.0, 1.0 ] );
	B = new Float64Array( [ 1.0, 0.0, 0.0, 1.0 ] );
	result = dlagv2( A, 1, 2, 0, B, 1, 2, 0, alphar, 1, 0, alphai, 1, 0, beta, 1, 0 ); // eslint-disable-line max-len
	assertArrayClose( toArray( A ), tc.A, 1e-14, 'A' );
	assertArrayClose( toArray( B ), tc.B, 1e-14, 'B' );
	assertArrayClose( toArray( alphar ), tc.ALPHAR, 1e-14, 'ALPHAR' );
	assertArrayClose( toArray( alphai ), tc.ALPHAI, 1e-14, 'ALPHAI' );
	assertArrayClose( toArray( beta ), tc.BETA, 1e-14, 'BETA' );
	assertClose( result.CSL, tc.CSL, 1e-14, 'CSL' );
	assertClose( result.SNL, tc.SNL, 1e-14, 'SNL' );
	assertClose( result.CSR, tc.CSR, 1e-14, 'CSR' );
	assertClose( result.SNR, tc.SNR, 1e-14, 'SNR' );
});

test( 'dlagv2: identity_b', function t() {
	var alphar;
	var alphai;
	var result;
	var beta;
	var tc;
	var A;
	var B;

	alphar = new Float64Array( 2 );
	alphai = new Float64Array( 2 );
	beta = new Float64Array( 2 );
	tc = findCase( 'identity_b' );
	A = new Float64Array( [ 5.0, 1.0, 2.0, 4.0 ] );
	B = new Float64Array( [ 1.0, 0.0, 0.0, 1.0 ] );
	result = dlagv2( A, 1, 2, 0, B, 1, 2, 0, alphar, 1, 0, alphai, 1, 0, beta, 1, 0 ); // eslint-disable-line max-len
	assertArrayClose( toArray( A ), tc.A, 1e-14, 'A' );
	assertArrayClose( toArray( B ), tc.B, 1e-14, 'B' );
	assertArrayClose( toArray( alphar ), tc.ALPHAR, 1e-14, 'ALPHAR' );
	assertArrayClose( toArray( alphai ), tc.ALPHAI, 1e-14, 'ALPHAI' );
	assertArrayClose( toArray( beta ), tc.BETA, 1e-14, 'BETA' );
	assertClose( result.CSL, tc.CSL, 1e-14, 'CSL' );
	assertClose( result.SNL, tc.SNL, 1e-14, 'SNL' );
	assertClose( result.CSR, tc.CSR, 1e-14, 'CSR' );
	assertClose( result.SNR, tc.SNR, 1e-14, 'SNR' );
});

test( 'dlagv2: distinct_real', function t() {
	var alphar;
	var alphai;
	var result;
	var beta;
	var tc;
	var A;
	var B;

	alphar = new Float64Array( 2 );
	alphai = new Float64Array( 2 );
	beta = new Float64Array( 2 );
	tc = findCase( 'distinct_real' );
	A = new Float64Array( [ 6.0, 2.0, 3.0, 7.0 ] );
	B = new Float64Array( [ 3.0, 0.0, 1.0, 2.0 ] );
	result = dlagv2( A, 1, 2, 0, B, 1, 2, 0, alphar, 1, 0, alphai, 1, 0, beta, 1, 0 ); // eslint-disable-line max-len
	assertArrayClose( toArray( A ), tc.A, 1e-14, 'A' );
	assertArrayClose( toArray( B ), tc.B, 1e-14, 'B' );
	assertArrayClose( toArray( alphar ), tc.ALPHAR, 1e-14, 'ALPHAR' );
	assertArrayClose( toArray( alphai ), tc.ALPHAI, 1e-14, 'ALPHAI' );
	assertArrayClose( toArray( beta ), tc.BETA, 1e-14, 'BETA' );
	assertClose( result.CSL, tc.CSL, 1e-14, 'CSL' );
	assertClose( result.SNL, tc.SNL, 1e-14, 'SNL' );
	assertClose( result.CSR, tc.CSR, 1e-14, 'CSR' );
	assertClose( result.SNR, tc.SNR, 1e-14, 'SNR' );
});

test( 'dlagv2: negative_complex', function t() {
	var alphar;
	var alphai;
	var result;
	var beta;
	var tc;
	var A;
	var B;

	alphar = new Float64Array( 2 );
	alphai = new Float64Array( 2 );
	beta = new Float64Array( 2 );
	tc = findCase( 'negative_complex' );
	A = new Float64Array( [ -2.0, 5.0, -4.0, -2.0 ] );
	B = new Float64Array( [ 2.0, 0.0, 1.0, 3.0 ] );
	result = dlagv2( A, 1, 2, 0, B, 1, 2, 0, alphar, 1, 0, alphai, 1, 0, beta, 1, 0 ); // eslint-disable-line max-len
	assertArrayClose( toArray( A ), tc.A, 1e-14, 'A' );
	assertArrayClose( toArray( B ), tc.B, 1e-14, 'B' );
	assertArrayClose( toArray( alphar ), tc.ALPHAR, 1e-14, 'ALPHAR' );
	assertArrayClose( toArray( alphai ), tc.ALPHAI, 1e-14, 'ALPHAI' );
	assertArrayClose( toArray( beta ), tc.BETA, 1e-14, 'BETA' );
	assertClose( result.CSL, tc.CSL, 1e-14, 'CSL' );
	assertClose( result.SNL, tc.SNL, 1e-14, 'SNL' );
	assertClose( result.CSR, tc.CSR, 1e-14, 'CSR' );
	assertClose( result.SNR, tc.SNR, 1e-14, 'SNR' );
});

test( 'dlagv2: large_values', function t() {
	var alphar;
	var alphai;
	var result;
	var beta;
	var tc;
	var A;
	var B;

	alphar = new Float64Array( 2 );
	alphai = new Float64Array( 2 );
	beta = new Float64Array( 2 );
	tc = findCase( 'large_values' );
	A = new Float64Array( [ 1.0e10, 3.0e9, 5.0e9, 2.0e10 ] );
	B = new Float64Array( [ 1.0e10, 0.0, 4.0e9, 8.0e9 ] );
	result = dlagv2( A, 1, 2, 0, B, 1, 2, 0, alphar, 1, 0, alphai, 1, 0, beta, 1, 0 ); // eslint-disable-line max-len
	assertArrayClose( toArray( A ), tc.A, 1e-14, 'A' );
	assertArrayClose( toArray( B ), tc.B, 1e-14, 'B' );
	assertArrayClose( toArray( alphar ), tc.ALPHAR, 1e-14, 'ALPHAR' );
	assertArrayClose( toArray( alphai ), tc.ALPHAI, 1e-14, 'ALPHAI' );
	assertArrayClose( toArray( beta ), tc.BETA, 1e-14, 'BETA' );
	assertClose( result.CSL, tc.CSL, 1e-14, 'CSL' );
	assertClose( result.SNL, tc.SNL, 1e-14, 'SNL' );
	assertClose( result.CSR, tc.CSR, 1e-14, 'CSR' );
	assertClose( result.SNR, tc.SNR, 1e-14, 'SNR' );
});

test( 'dlagv2: diagonal_a', function t() {
	var alphar;
	var alphai;
	var result;
	var beta;
	var tc;
	var A;
	var B;

	alphar = new Float64Array( 2 );
	alphai = new Float64Array( 2 );
	beta = new Float64Array( 2 );
	tc = findCase( 'diagonal_a' );
	A = new Float64Array( [ 3.0, 0.0, 0.0, 7.0 ] );
	B = new Float64Array( [ 2.0, 0.0, 1.0, 5.0 ] );
	result = dlagv2( A, 1, 2, 0, B, 1, 2, 0, alphar, 1, 0, alphai, 1, 0, beta, 1, 0 ); // eslint-disable-line max-len
	assertArrayClose( toArray( A ), tc.A, 1e-14, 'A' );
	assertArrayClose( toArray( B ), tc.B, 1e-14, 'B' );
	assertArrayClose( toArray( alphar ), tc.ALPHAR, 1e-14, 'ALPHAR' );
	assertArrayClose( toArray( alphai ), tc.ALPHAI, 1e-14, 'ALPHAI' );
	assertArrayClose( toArray( beta ), tc.BETA, 1e-14, 'BETA' );
	assertClose( result.CSL, tc.CSL, 1e-14, 'CSL' );
	assertClose( result.SNL, tc.SNL, 1e-14, 'SNL' );
	assertClose( result.CSR, tc.CSR, 1e-14, 'CSR' );
	assertClose( result.SNR, tc.SNR, 1e-14, 'SNR' );
});

test( 'dlagv2: real_rr_le_qq', function t() {
	var alphar;
	var alphai;
	var result;
	var beta;
	var tc;
	var A;
	var B;

	alphar = new Float64Array( 2 );
	alphai = new Float64Array( 2 );
	beta = new Float64Array( 2 );
	tc = findCase( 'real_rr_le_qq' );
	A = new Float64Array( [ 1.0, 10.0, 1.0, 2.0 ] );
	B = new Float64Array( [ 1.0, 0.0, 0.0, 1.0 ] );
	result = dlagv2( A, 1, 2, 0, B, 1, 2, 0, alphar, 1, 0, alphai, 1, 0, beta, 1, 0 ); // eslint-disable-line max-len
	assertArrayClose( toArray( A ), tc.A, 1e-14, 'A' );
	assertArrayClose( toArray( B ), tc.B, 1e-14, 'B' );
	assertArrayClose( toArray( alphar ), tc.ALPHAR, 1e-14, 'ALPHAR' );
	assertArrayClose( toArray( alphai ), tc.ALPHAI, 1e-14, 'ALPHAI' );
	assertArrayClose( toArray( beta ), tc.BETA, 1e-14, 'BETA' );
	assertClose( result.CSL, tc.CSL, 1e-14, 'CSL' );
	assertClose( result.SNL, tc.SNL, 1e-14, 'SNL' );
	assertClose( result.CSR, tc.CSR, 1e-14, 'CSR' );
	assertClose( result.SNR, tc.SNR, 1e-14, 'SNR' );
});
