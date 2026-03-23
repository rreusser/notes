'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var Float64Array = require( '@stdlib/array/float64' );
var path = require( 'path' );
var dsytrd = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dsytrd.jsonl' ), 'utf8' ).trim().split( '\n' );
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

/**
* Build the 4x4 SPD test matrix in column-major order.
*/
function matrix4x4() {
	// 4x4 symmetric:
	//  4  1  2  1
	//  1  5  1  2
	//  2  1  6  1
	//  1  2  1  7
	var A = new Float64Array( 16 );
	A[  0 ] = 4.0; A[  1 ] = 1.0; A[  2 ] = 2.0; A[  3 ] = 1.0;
	A[  4 ] = 1.0; A[  5 ] = 5.0; A[  6 ] = 1.0; A[  7 ] = 2.0;
	A[  8 ] = 2.0; A[  9 ] = 1.0; A[ 10 ] = 6.0; A[ 11 ] = 1.0;
	A[ 12 ] = 1.0; A[ 13 ] = 2.0; A[ 14 ] = 1.0; A[ 15 ] = 7.0;
	return A;
}

/**
* Build the 35x35 test matrix (same as Fortran test).
*/
function matrix35x35() {
	var A = new Float64Array( 35 * 35 );
	var i;
	var j;
	for ( j = 0; j < 35; j++ ) {
		for ( i = 0; i < 35; i++ ) {
			if ( i === j ) {
				A[ i + j * 35 ] = ( i + 1 ) + 35.0;
			} else {
				A[ i + j * 35 ] = 0.1 * ( ( ( i + 1 ) * ( j + 1 ) + ( i + 1 ) + ( j + 1 ) ) % 7 ) / 7.0;
			}
		}
	}
	// Ensure symmetric (upper triangle mirrors lower)
	for ( j = 0; j < 35; j++ ) {
		for ( i = j + 1; i < 35; i++ ) {
			A[ i + j * 35 ] = A[ j + i * 35 ];
		}
	}
	return A;
}

/**
* Extract d (diagonal), e (off-diagonal), tau arrays from dsytrd results.
*/
function extractResults( N, d, e, TAU ) {
	return {
		d: Array.prototype.slice.call( d, 0, N ),
		e: Array.prototype.slice.call( e, 0, N - 1 ),
		tau: Array.prototype.slice.call( TAU, 0, N - 1 )
	};
}


// TESTS //

test( 'dsytrd: upper_4x4 (unblocked path)', function t() {
	var tc = findCase( 'upper_4x4' );
	var TAU = new Float64Array( 3 );
	var d = new Float64Array( 4 );
	var e = new Float64Array( 3 );
	var A = matrix4x4();
	var WORK = new Float64Array( 1 );
	var info;
	var res;

	info = dsytrd('upper', 4, A, 1, 4, 0, d, 1, 0, e, 1, 0, TAU, 1, 0 );
	assert.equal( info, 0, 'info' );
	res = extractResults( 4, d, e, TAU );
	assertArrayClose( res.d, tc.d, 1e-14, 'd' );
	assertArrayClose( res.e, tc.e, 1e-14, 'e' );
	assertArrayClose( res.tau, tc.tau, 1e-14, 'tau' );
});

test( 'dsytrd: lower_4x4 (unblocked path)', function t() {
	var tc = findCase( 'lower_4x4' );
	var TAU = new Float64Array( 3 );
	var d = new Float64Array( 4 );
	var e = new Float64Array( 3 );
	var A = matrix4x4();
	var WORK = new Float64Array( 1 );
	var info;
	var res;

	info = dsytrd('lower', 4, A, 1, 4, 0, d, 1, 0, e, 1, 0, TAU, 1, 0 );
	assert.equal( info, 0, 'info' );
	res = extractResults( 4, d, e, TAU );
	assertArrayClose( res.d, tc.d, 1e-14, 'd' );
	assertArrayClose( res.e, tc.e, 1e-14, 'e' );
	assertArrayClose( res.tau, tc.tau, 1e-14, 'tau' );
});

test( 'dsytrd: n_one_upper', function t() {
	var tc = findCase( 'n_one_upper' );
	var TAU = new Float64Array( 1 );
	var d = new Float64Array( 1 );
	var e = new Float64Array( 1 );
	var A = new Float64Array( [ 3.0 ] );
	var WORK = new Float64Array( 1 );
	var info;

	info = dsytrd('upper', 1, A, 1, 1, 0, d, 1, 0, e, 1, 0, TAU, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertClose( d[ 0 ], tc.d1, 1e-14, 'd1' );
});

test( 'dsytrd: n_one_lower', function t() {
	var tc = findCase( 'n_one_lower' );
	var TAU = new Float64Array( 1 );
	var d = new Float64Array( 1 );
	var e = new Float64Array( 1 );
	var A = new Float64Array( [ 5.0 ] );
	var WORK = new Float64Array( 1 );
	var info;

	info = dsytrd('lower', 1, A, 1, 1, 0, d, 1, 0, e, 1, 0, TAU, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertClose( d[ 0 ], tc.d1, 1e-14, 'd1' );
});

test( 'dsytrd: n_zero', function t() {
	var A = new Float64Array( 1 );
	var d = new Float64Array( 1 );
	var e = new Float64Array( 1 );
	var TAU = new Float64Array( 1 );
	var WORK = new Float64Array( 1 );
	var info;

	info = dsytrd('upper', 0, A, 1, 1, 0, d, 1, 0, e, 1, 0, TAU, 1, 0 );
	assert.equal( info, 0, 'info' );
});

test( 'dsytrd: upper_35x35 (blocked path)', function t() {
	var tc = findCase( 'upper_35x35' );
	var TAU = new Float64Array( 34 );
	var d = new Float64Array( 35 );
	var e = new Float64Array( 34 );
	var A = matrix35x35();
	var WORK = new Float64Array( 1 );
	var info;
	var res;

	info = dsytrd('upper', 35, A, 1, 35, 0, d, 1, 0, e, 1, 0, TAU, 1, 0 );
	assert.equal( info, 0, 'info' );
	res = extractResults( 35, d, e, TAU );
	assertArrayClose( res.d, tc.d, 1e-12, 'd' );
	assertArrayClose( res.e, tc.e, 1e-12, 'e' );
	assertArrayClose( res.tau, tc.tau, 1e-12, 'tau' );
});

test( 'dsytrd: lower_35x35 (blocked path)', function t() {
	var tc = findCase( 'lower_35x35' );
	var TAU = new Float64Array( 34 );
	var d = new Float64Array( 35 );
	var e = new Float64Array( 34 );
	var A = matrix35x35();
	var WORK = new Float64Array( 1 );
	var info;
	var res;

	info = dsytrd('lower', 35, A, 1, 35, 0, d, 1, 0, e, 1, 0, TAU, 1, 0 );
	assert.equal( info, 0, 'info' );
	res = extractResults( 35, d, e, TAU );
	assertArrayClose( res.d, tc.d, 1e-12, 'd' );
	assertArrayClose( res.e, tc.e, 1e-12, 'e' );
	assertArrayClose( res.tau, tc.tau, 1e-12, 'tau' );
});
