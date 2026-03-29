/* eslint-disable no-restricted-syntax, stdlib/require-globals, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dtpmv = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dtpmv.jsonl' ), 'utf8' ).trim().split( '\n' );
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

// 4x4 upper triangular matrix:
//   [ 2  3  5  8 ]
//   [ 0  4  6  9 ]
//   [ 0  0  7 10 ]
//   [ 0  0  0 11 ]
// Upper packed (column-major): 2, 3, 4, 5, 6, 7, 8, 9, 10, 11

// 4x4 lower triangular matrix:
//   [ 2  0  0  0 ]
//   [ 3  5  0  0 ]
//   [ 4  6  8  0 ]
//   [ 7  9 10 11 ]
// Lower packed (column-major): 2, 3, 4, 7, 5, 6, 9, 8, 10, 11

test( 'dtpmv: upper_notrans_nonunit', function t() {
	var tc = findCase( 'upper_notrans_nonunit' );
	var AP = new Float64Array( [ 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 ] );
	var x = new Float64Array( [ 1, 2, 3, 4 ] );
	dtpmv( 'upper', 'no-transpose', 'non-unit', 4, AP, 1, 0, x, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'dtpmv: upper_trans_nonunit', function t() {
	var tc = findCase( 'upper_trans_nonunit' );
	var AP = new Float64Array( [ 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 ] );
	var x = new Float64Array( [ 1, 2, 3, 4 ] );
	dtpmv( 'upper', 'transpose', 'non-unit', 4, AP, 1, 0, x, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'dtpmv: upper_notrans_unit', function t() {
	var tc = findCase( 'upper_notrans_unit' );
	var AP = new Float64Array( [ 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 ] );
	var x = new Float64Array( [ 1, 2, 3, 4 ] );
	dtpmv( 'upper', 'no-transpose', 'unit', 4, AP, 1, 0, x, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'dtpmv: lower_notrans_nonunit', function t() {
	var tc = findCase( 'lower_notrans_nonunit' );
	var AP = new Float64Array( [ 2, 3, 4, 7, 5, 6, 9, 8, 10, 11 ] );
	var x = new Float64Array( [ 1, 2, 3, 4 ] );
	dtpmv( 'lower', 'no-transpose', 'non-unit', 4, AP, 1, 0, x, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'dtpmv: lower_trans_nonunit', function t() {
	var tc = findCase( 'lower_trans_nonunit' );
	var AP = new Float64Array( [ 2, 3, 4, 7, 5, 6, 9, 8, 10, 11 ] );
	var x = new Float64Array( [ 1, 2, 3, 4 ] );
	dtpmv( 'lower', 'transpose', 'non-unit', 4, AP, 1, 0, x, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'dtpmv: n_zero', function t() {
	var AP = new Float64Array( [ 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 ] );
	var x = new Float64Array( [ 99.0 ] );
	dtpmv( 'upper', 'no-transpose', 'non-unit', 0, AP, 1, 0, x, 1, 0 );
	assert.equal( x[ 0 ], 99.0 );
});

test( 'dtpmv: stride', function t() {
	var tc = findCase( 'stride' );
	var AP = new Float64Array( [ 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 ] );
	var x = new Float64Array( [ 1, 0, 2, 0, 3, 0, 4, 0 ] );
	dtpmv( 'upper', 'no-transpose', 'non-unit', 4, AP, 1, 0, x, 2, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});
