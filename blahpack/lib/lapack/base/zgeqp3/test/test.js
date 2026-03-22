'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var zgeqp3 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zgeqp3.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// VARIABLES //

var LDA = 8; // Matches Fortran MAXMN


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

test( 'zgeqp3: 4x3 matrix', function t() {
	var tc = findCase( 'rect_4x3' );
	var A = new Float64Array( 2 * LDA * LDA );
	// A = [1 0+2i 3+i; 2+i 1 0; 0 3+i 1; 1+i 2 2+i]
	A[0]=1; A[1]=0; A[2]=2; A[3]=1; A[4]=0; A[5]=0; A[6]=1; A[7]=1;
	A[2*LDA]=0; A[2*LDA+1]=2; A[2*LDA+2]=1; A[2*LDA+3]=0; A[2*LDA+4]=3; A[2*LDA+5]=1; A[2*LDA+6]=2; A[2*LDA+7]=0;
	A[4*LDA]=3; A[4*LDA+1]=1; A[4*LDA+2]=0; A[4*LDA+3]=0; A[4*LDA+4]=1; A[4*LDA+5]=0; A[4*LDA+6]=2; A[4*LDA+7]=1;

	var JPVT = new Int32Array( 3 );
	var TAU = new Float64Array( 6 );
	var WORK = new Float64Array( 400 );
	var RWORK = new Float64Array( 2 * LDA );

	var info = zgeqp3( 4, 3, A, 1, LDA, 0, JPVT, 1, 0, TAU, 1, 0, WORK, 1, 0, 200, RWORK, 1, 0 );

	assertClose( info, tc.info, 1e-14, 'info' );
	assertArrayClose( Array.from( A.subarray( 0, tc.a.length ) ), tc.a, 1e-10, 'a' );
	assertArrayClose( Array.from( TAU ), tc.tau, 1e-10, 'tau' );
	assert.deepStrictEqual( Array.from( JPVT ), tc.jpvt, 'jpvt' );
});

test( 'zgeqp3: 3x4 matrix', function t() {
	var tc = findCase( 'rect_3x4' );
	var A = new Float64Array( 2 * LDA * LDA );
	A[0]=1; A[1]=0; A[2]=0; A[3]=1; A[4]=2; A[5]=0;
	A[2*LDA]=3; A[2*LDA+1]=1; A[2*LDA+2]=1; A[2*LDA+3]=0; A[2*LDA+4]=0; A[2*LDA+5]=1;
	A[4*LDA]=0; A[4*LDA+1]=2; A[4*LDA+2]=2; A[4*LDA+3]=1; A[4*LDA+4]=1; A[4*LDA+5]=0;
	A[6*LDA]=1; A[6*LDA+1]=1; A[6*LDA+2]=0; A[6*LDA+3]=0; A[6*LDA+4]=3; A[6*LDA+5]=0;

	var JPVT = new Int32Array( 4 );
	var TAU = new Float64Array( 6 );
	var WORK = new Float64Array( 400 );
	var RWORK = new Float64Array( 2 * LDA );

	var info = zgeqp3( 3, 4, A, 1, LDA, 0, JPVT, 1, 0, TAU, 1, 0, WORK, 1, 0, 200, RWORK, 1, 0 );

	assertClose( info, tc.info, 1e-14, 'info' );
	assertArrayClose( Array.from( A.subarray( 0, tc.a.length ) ), tc.a, 1e-10, 'a' );
	assertArrayClose( Array.from( TAU ), tc.tau, 1e-10, 'tau' );
	assert.deepStrictEqual( Array.from( JPVT ), tc.jpvt, 'jpvt' );
});

test( 'zgeqp3: rank-deficient 3x3', function t() {
	var A = new Float64Array( 2 * LDA * LDA );
	// col 3 = col 1 + col 2, so rank = 1
	A[0]=1; A[1]=0; A[2]=2; A[3]=0; A[4]=3; A[5]=0;
	A[2*LDA]=0; A[2*LDA+1]=1; A[2*LDA+2]=0; A[2*LDA+3]=2; A[2*LDA+4]=0; A[2*LDA+5]=3;
	A[4*LDA]=1; A[4*LDA+1]=1; A[4*LDA+2]=2; A[4*LDA+3]=2; A[4*LDA+4]=3; A[4*LDA+5]=3;

	var JPVT = new Int32Array( 3 );
	var TAU = new Float64Array( 6 );
	var WORK = new Float64Array( 400 );
	var RWORK = new Float64Array( 2 * LDA );

	var info = zgeqp3( 3, 3, A, 1, LDA, 0, JPVT, 1, 0, TAU, 1, 0, WORK, 1, 0, 200, RWORK, 1, 0 );

	assertClose( info, 0, 1e-14, 'info' );
	// First column must be col 3 (largest norm), columns 2-3 can be either order
	assert.equal( JPVT[ 0 ], 3, 'first pivot is column 3' );
	// R(0,0) should be -sqrt(28) ~ -5.2915
	assertClose( A[ 0 ], -5.2915026221291805, 1e-10, 'R(0,0)' );
	// R(1,1) and R(2,2) should be ~0 (rank deficient)
	assertClose( Math.abs( A[ 2 * LDA + 2 ] ), 0.0, 1e-10, '|R(1,1)|~0' );
	assertClose( Math.abs( A[ 4 * LDA + 4 ] ), 0.0, 1e-10, '|R(2,2)|~0' );
});

test( 'zgeqp3: N=0 quick return', function t() {
	var tc = findCase( 'n_zero' );
	var A = new Float64Array( 20 );
	var JPVT = new Int32Array( 1 );
	var TAU = new Float64Array( 2 );
	var WORK = new Float64Array( 20 );
	var RWORK = new Float64Array( 10 );

	var info = zgeqp3( 3, 0, A, 1, 3, 0, JPVT, 1, 0, TAU, 1, 0, WORK, 1, 0, 10, RWORK, 1, 0 );
	assertClose( info, tc.info, 1e-14, 'info' );
});

test( 'zgeqp3: M=0 quick return', function t() {
	var tc = findCase( 'm_zero' );
	var A = new Float64Array( 20 );
	var JPVT = new Int32Array( 3 );
	var TAU = new Float64Array( 2 );
	var WORK = new Float64Array( 20 );
	var RWORK = new Float64Array( 10 );

	var info = zgeqp3( 0, 3, A, 1, 1, 0, JPVT, 1, 0, TAU, 1, 0, WORK, 1, 0, 10, RWORK, 1, 0 );
	assertClose( info, tc.info, 1e-14, 'info' );
});

test( 'zgeqp3: 1x1 matrix', function t() {
	var tc = findCase( 'one_by_one' );
	var A = new Float64Array( 2 * LDA * LDA );
	A[0] = 3; A[1] = 4;

	var JPVT = new Int32Array( [ 0 ] );
	var TAU = new Float64Array( 2 );
	var WORK = new Float64Array( 400 );
	var RWORK = new Float64Array( 2 * LDA );

	var info = zgeqp3( 1, 1, A, 1, LDA, 0, JPVT, 1, 0, TAU, 1, 0, WORK, 1, 0, 200, RWORK, 1, 0 );

	assertClose( info, tc.info, 1e-14, 'info' );
	assertArrayClose( Array.from( A.subarray( 0, tc.a.length ) ), tc.a, 1e-10, 'a' );
	assertArrayClose( Array.from( TAU ), tc.tau, 1e-10, 'tau' );
	assert.deepStrictEqual( Array.from( JPVT ), tc.jpvt, 'jpvt' );
});

test( 'zgeqp3: fixed column', function t() {
	var tc = findCase( 'fixed_col' );
	var A = new Float64Array( 2 * LDA * LDA );
	A[0]=1; A[1]=0; A[2]=0; A[3]=0; A[4]=0; A[5]=0;
	A[2*LDA]=0; A[2*LDA+1]=0; A[2*LDA+2]=3; A[2*LDA+3]=0; A[2*LDA+4]=4; A[2*LDA+5]=0;
	A[4*LDA]=0; A[4*LDA+1]=0; A[4*LDA+2]=1; A[4*LDA+3]=1; A[4*LDA+4]=2; A[4*LDA+5]=0;

	var JPVT = new Int32Array( [ 1, 0, 0 ] ); // Fix column 1
	var TAU = new Float64Array( 6 );
	var WORK = new Float64Array( 400 );
	var RWORK = new Float64Array( 2 * LDA );

	var info = zgeqp3( 3, 3, A, 1, LDA, 0, JPVT, 1, 0, TAU, 1, 0, WORK, 1, 0, 200, RWORK, 1, 0 );

	assertClose( info, tc.info, 1e-14, 'info' );
	assertArrayClose( Array.from( A.subarray( 0, tc.a.length ) ), tc.a, 1e-10, 'a' );
	assertArrayClose( Array.from( TAU ), tc.tau, 1e-10, 'tau' );
	assert.deepStrictEqual( Array.from( JPVT ), tc.jpvt, 'jpvt' );
});
