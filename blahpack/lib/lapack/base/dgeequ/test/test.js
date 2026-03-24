'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dgeequ = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dgeequ.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dgeequ: basic 3x3 well-conditioned matrix', function t() {
	var tc = findCase( 'basic' );
	var A = new Float64Array( [ 4.0, 1.0, 0.5, 1.0, 3.0, 1.0, 0.5, 1.0, 2.0 ] );
	var r = new Float64Array( 3 );
	var c = new Float64Array( 3 );
	var result = dgeequ( 3, 3, A, 1, 3, 0, r, 1, 0, c, 1, 0 );
	assert.equal( result.info, tc.info );
	assertArrayClose( r, tc.r, 1e-14, 'r' );
	assertArrayClose( c, tc.c, 1e-14, 'c' );
	assertClose( result.rowcnd, tc.rowcnd, 1e-14, 'rowcnd' );
	assertClose( result.colcnd, tc.colcnd, 1e-14, 'colcnd' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
});

test( 'dgeequ: diagonal matrix with varying scales', function t() {
	var tc = findCase( 'diagonal_varied' );
	var A = new Float64Array( 9 );
	A[ 0 ] = 100.0; A[ 4 ] = 1.0; A[ 8 ] = 0.01;
	var r = new Float64Array( 3 );
	var c = new Float64Array( 3 );
	var result = dgeequ( 3, 3, A, 1, 3, 0, r, 1, 0, c, 1, 0 );
	assert.equal( result.info, tc.info );
	assertArrayClose( r, tc.r, 1e-14, 'r' );
	assertArrayClose( c, tc.c, 1e-14, 'c' );
	assertClose( result.rowcnd, tc.rowcnd, 1e-14, 'rowcnd' );
	assertClose( result.colcnd, tc.colcnd, 1e-14, 'colcnd' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
});

test( 'dgeequ: matrix with zero row returns info=i', function t() {
	var tc = findCase( 'zero_row' );
	var A = new Float64Array( [ 1.0, 0.0, 1.0, 2.0, 0.0, 3.0, 4.0, 0.0, 5.0 ] );
	var r = new Float64Array( 3 );
	var c = new Float64Array( 3 );
	var result = dgeequ( 3, 3, A, 1, 3, 0, r, 1, 0, c, 1, 0 );
	assert.equal( result.info, tc.info );
});

test( 'dgeequ: matrix with zero column returns info=M+j', function t() {
	var tc = findCase( 'zero_col' );
	var A = new Float64Array( [ 1.0, 2.0, 3.0, 0.0, 0.0, 0.0, 4.0, 5.0, 6.0 ] );
	var r = new Float64Array( 3 );
	var c = new Float64Array( 3 );
	var result = dgeequ( 3, 3, A, 1, 3, 0, r, 1, 0, c, 1, 0 );
	assert.equal( result.info, tc.info );
	assertArrayClose( r, tc.r, 1e-14, 'r' );
});

test( 'dgeequ: identity matrix', function t() {
	var tc = findCase( 'identity' );
	var A = new Float64Array( 9 );
	A[ 0 ] = 1.0; A[ 4 ] = 1.0; A[ 8 ] = 1.0;
	var r = new Float64Array( 3 );
	var c = new Float64Array( 3 );
	var result = dgeequ( 3, 3, A, 1, 3, 0, r, 1, 0, c, 1, 0 );
	assert.equal( result.info, tc.info );
	assertArrayClose( r, tc.r, 1e-14, 'r' );
	assertArrayClose( c, tc.c, 1e-14, 'c' );
	assertClose( result.rowcnd, tc.rowcnd, 1e-14, 'rowcnd' );
	assertClose( result.colcnd, tc.colcnd, 1e-14, 'colcnd' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
});

test( 'dgeequ: quick return M=0', function t() {
	var tc = findCase( 'm_zero' );
	var r = new Float64Array( 0 );
	var c = new Float64Array( 3 );
	var result = dgeequ( 0, 3, new Float64Array( 0 ), 1, 1, 0, r, 1, 0, c, 1, 0 );
	assert.equal( result.info, 0 );
	assertClose( result.rowcnd, tc.rowcnd, 1e-14, 'rowcnd' );
	assertClose( result.colcnd, tc.colcnd, 1e-14, 'colcnd' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
});

test( 'dgeequ: quick return N=0', function t() {
	var tc = findCase( 'n_zero' );
	var r = new Float64Array( 3 );
	var c = new Float64Array( 0 );
	var result = dgeequ( 3, 0, new Float64Array( 0 ), 1, 1, 0, r, 1, 0, c, 1, 0 );
	assert.equal( result.info, 0 );
	assertClose( result.rowcnd, tc.rowcnd, 1e-14, 'rowcnd' );
	assertClose( result.colcnd, tc.colcnd, 1e-14, 'colcnd' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
});

test( 'dgeequ: non-square 2x4 matrix', function t() {
	var tc = findCase( 'nonsquare' );
	var A = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0 ] );
	var r = new Float64Array( 2 );
	var c = new Float64Array( 4 );
	var result = dgeequ( 2, 4, A, 1, 2, 0, r, 1, 0, c, 1, 0 );
	assert.equal( result.info, tc.info );
	assertArrayClose( r, tc.r, 1e-14, 'r' );
	assertArrayClose( c, tc.c, 1e-14, 'c' );
	assertClose( result.rowcnd, tc.rowcnd, 1e-14, 'rowcnd' );
	assertClose( result.colcnd, tc.colcnd, 1e-14, 'colcnd' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
});
