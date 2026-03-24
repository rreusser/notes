

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var dgehrd = require( '../../dgehrd/lib/base.js' );
var dorghr = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dorghr.jsonl' ), 'utf8' ).trim().split( '\n' );
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
* Extracts a column-major N-by-N submatrix from A into a flat array.
*/
function extractColMajor( A, strideA1, strideA2, offsetA, N ) {
	var out = [];
	var i;
	var j;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < N; i++ ) {
			out.push( A[ offsetA + ( i * strideA1 ) + ( j * strideA2 ) ] );
		}
	}
	return out;
}

/**
* Runs dgehrd then dorghr and returns the Q matrix as a flat column-major array.
*/
function runDorghr( N, ilo, ihi, Ainput ) {
	var WORK = new Float64Array( 1024 );
	var TAU = new Float64Array( Math.max( 1, N ) );
	var A = new Float64Array( N * N );
	var info;
	var i;

	// Copy input into A (column-major)
	for ( i = 0; i < Ainput.length; i++ ) {
		A[ i ] = Ainput[ i ];
	}

	// Run dgehrd: ilo and ihi are 1-based
	dgehrd( N, ilo, ihi, A, 1, N, 0, TAU, 1, 0, WORK, 1, 0 );

	// Run dorghr
	info = dorghr( N, ilo, ihi, A, 1, N, 0, TAU, 1, 0, WORK, 1, 0, 0 );

	return { A: A, info: info };
}


// TESTS //

test( 'dorghr: 5x5_full (ILO=1, IHI=5)', function t() {
	var tc = findCase( '5x5_full' );
	// Column-major 5x5 input
	var Ainput = [
		2, 1, 3, 1, 2,
		1, 4, 2, 3, 1,
		3, 2, 5, 2, 3,
		1, 3, 2, 4, 1,
		2, 1, 3, 1, 5
	];
	var result = runDorghr( 5, 1, 5, Ainput );
	var Q = extractColMajor( result.A, 1, 5, 0, 5 );
	assert.equal( result.info, 0, 'INFO' );
	assertArrayClose( Q, tc.Q, 1e-12, 'Q' );
});

test( 'dorghr: 5x5_partial (ILO=2, IHI=4)', function t() {
	var tc = findCase( '5x5_partial' );
	var Ainput = [
		2, 1, 3, 1, 2,
		1, 4, 2, 3, 1,
		3, 2, 5, 2, 3,
		1, 3, 2, 4, 1,
		2, 1, 3, 1, 5
	];
	var result = runDorghr( 5, 2, 4, Ainput );
	var Q = extractColMajor( result.A, 1, 5, 0, 5 );
	assert.equal( result.info, 0, 'INFO' );
	assertArrayClose( Q, tc.Q, 1e-12, 'Q' );
});

test( 'dorghr: n_zero (N=0)', function t() {
	var WORK = new Float64Array( 1 );
	var TAU = new Float64Array( 1 );
	var A = new Float64Array( 1 );
	var info = dorghr( 0, 1, 0, A, 1, 1, 0, TAU, 1, 0, WORK, 1, 0, 0 );
	assert.equal( info, 0, 'INFO' );
});

test( 'dorghr: n_one (N=1)', function t() {
	var tc = findCase( 'n_one' );
	var Ainput = [ 99.0 ];
	var result = runDorghr( 1, 1, 1, Ainput );
	var Q = extractColMajor( result.A, 1, 1, 0, 1 );
	assert.equal( result.info, 0, 'INFO' );
	assertArrayClose( Q, tc.Q, 1e-12, 'Q' );
});

test( 'dorghr: ilo_eq_ihi (ILO=IHI=2, N=4)', function t() {
	var tc = findCase( 'ilo_eq_ihi' );
	// Upper triangular-ish matrix, column-major
	var Ainput = [
		1, 0, 0, 0,
		2, 5, 0, 0,
		3, 6, 8, 0,
		4, 7, 9, 10
	];
	var result = runDorghr( 4, 2, 2, Ainput );
	var Q = extractColMajor( result.A, 1, 4, 0, 4 );
	assert.equal( result.info, 0, 'INFO' );
	assertArrayClose( Q, tc.Q, 1e-12, 'Q' );
});

test( 'dorghr: 4x4_full (ILO=1, IHI=4)', function t() {
	var tc = findCase( '4x4_full' );
	var Ainput = [
		1, 5, 9, 13,
		2, 6, 10, 14,
		3, 7, 11, 15,
		4, 8, 12, 16
	];
	var result = runDorghr( 4, 1, 4, Ainput );
	var Q = extractColMajor( result.A, 1, 4, 0, 4 );
	assert.equal( result.info, 0, 'INFO' );
	assertArrayClose( Q, tc.Q, 1e-12, 'Q' );
});

test( 'dorghr: orthogonality check for 5x5_full', function t() {
	var Ainput = [
		2, 1, 3, 1, 2,
		1, 4, 2, 3, 1,
		3, 2, 5, 2, 3,
		1, 3, 2, 4, 1,
		2, 1, 3, 1, 5
	];
	var result = runDorghr( 5, 1, 5, Ainput );
	var N = 5;
	var A = result.A;
	var i;
	var j;
	var k;
	var dot;

	// Check Q^T * Q ≈ I
	for ( i = 0; i < N; i++ ) {
		for ( j = 0; j < N; j++ ) {
			dot = 0.0;
			for ( k = 0; k < N; k++ ) {
				dot += A[ k + i * N ] * A[ k + j * N ];
			}
			if ( i === j ) {
				assertClose( dot, 1.0, 1e-12, 'QtQ[' + i + ',' + j + ']' );
			} else {
				assertClose( dot, 0.0, 1e-12, 'QtQ[' + i + ',' + j + ']' );
			}
		}
	}
});
