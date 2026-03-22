'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var zgeqr2 = require( '../../zgeqr2/lib/base.js' );
var zunmqr = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zunmqr.jsonl' ), 'utf8' ).trim().split( '\n' );
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
* Compute QR of the 4x3 test matrix in a 6x6 container (LDA=6).
*/
function qr4x3() {
	var LDA = 6;
	var A = new Float64Array( 2 * LDA * 6 );
	A[0]=1; A[1]=0; A[2]=2; A[3]=1; A[4]=0; A[5]=0; A[6]=1; A[7]=1;
	A[2*LDA]=0; A[2*LDA+1]=2; A[2*LDA+2]=1; A[2*LDA+3]=0; A[2*LDA+4]=3; A[2*LDA+5]=1; A[2*LDA+6]=2; A[2*LDA+7]=0;
	A[4*LDA]=3; A[4*LDA+1]=1; A[4*LDA+2]=0; A[4*LDA+3]=0; A[4*LDA+4]=1; A[4*LDA+5]=0; A[4*LDA+6]=2; A[4*LDA+7]=1;
	var TAU = new Float64Array( 6 );
	var WORK = new Float64Array( 40 );
	zgeqr2( 4, 3, A, 1, LDA, 0, TAU, 1, 0, WORK, 1, 0 );
	return { A: A, TAU: TAU, LDA: LDA };
}

/**
* Create 4x4 identity in a 6x6 container (LDC=6, complex).
*/
function eye4in6() {
	var LDC = 6;
	var C = new Float64Array( 2 * LDC * 6 );
	C[ 2 * ( 0 + 0 * LDC ) ] = 1.0;
	C[ 2 * ( 1 + 1 * LDC ) ] = 1.0;
	C[ 2 * ( 2 + 2 * LDC ) ] = 1.0;
	C[ 2 * ( 3 + 3 * LDC ) ] = 1.0;
	return C;
}


// TESTS //

test( 'zunmqr: left, no transpose (Q*I)', function t() {
	var tc = findCase( 'left_notrans_4x4' );
	var qr = qr4x3();
	var LDC = 6;
	var C = eye4in6();
	var WORK = new Float64Array( 400 );
	var info = zunmqr( 'L', 'N', 4, 4, 3, qr.A, 1, qr.LDA, 0, qr.TAU, 1, 0, C, 1, LDC, 0, WORK, 1, 0, 200 );
	assertClose( info, tc.info, 1e-14, 'info' );
	// Fixture has first 32 doubles of C_r (flat layout of C(6,6))
	assertArrayClose( Array.from( C.subarray( 0, tc.c.length ) ), tc.c, 1e-10, 'c' );
});

test( 'zunmqr: left, conjugate transpose (Q^H*I)', function t() {
	var tc = findCase( 'left_conjtrans_4x4' );
	var qr = qr4x3();
	var LDC = 6;
	var C = eye4in6();
	var WORK = new Float64Array( 400 );
	var info = zunmqr( 'L', 'C', 4, 4, 3, qr.A, 1, qr.LDA, 0, qr.TAU, 1, 0, C, 1, LDC, 0, WORK, 1, 0, 200 );
	assertClose( info, tc.info, 1e-14, 'info' );
	assertArrayClose( Array.from( C.subarray( 0, tc.c.length ) ), tc.c, 1e-10, 'c' );
});

test( 'zunmqr: right, no transpose (I*Q)', function t() {
	var tc = findCase( 'right_notrans_4x4' );
	var qr = qr4x3();
	var LDC = 6;
	var C = eye4in6();
	var WORK = new Float64Array( 400 );
	var info = zunmqr( 'R', 'N', 4, 4, 3, qr.A, 1, qr.LDA, 0, qr.TAU, 1, 0, C, 1, LDC, 0, WORK, 1, 0, 200 );
	assertClose( info, tc.info, 1e-14, 'info' );
	assertArrayClose( Array.from( C.subarray( 0, tc.c.length ) ), tc.c, 1e-10, 'c' );
});

test( 'zunmqr: M=0 quick return', function t() {
	var tc = findCase( 'm_zero' );
	var A = new Float64Array( 10 );
	var TAU = new Float64Array( 4 );
	var C = new Float64Array( 10 );
	var WORK = new Float64Array( 40 );
	var info = zunmqr( 'L', 'N', 0, 4, 0, A, 1, 1, 0, TAU, 1, 0, C, 1, 1, 0, WORK, 1, 0, 20 );
	assertClose( info, tc.info, 1e-14, 'info' );
});

test( 'zunmqr: N=0 quick return', function t() {
	var tc = findCase( 'n_zero' );
	var A = new Float64Array( 10 );
	var TAU = new Float64Array( 4 );
	var C = new Float64Array( 10 );
	var WORK = new Float64Array( 40 );
	var info = zunmqr( 'L', 'N', 4, 0, 0, A, 1, 4, 0, TAU, 1, 0, C, 1, 4, 0, WORK, 1, 0, 20 );
	assertClose( info, tc.info, 1e-14, 'info' );
});

test( 'zunmqr: K=0 quick return', function t() {
	var tc = findCase( 'k_zero' );
	var A = new Float64Array( 10 );
	var TAU = new Float64Array( 4 );
	var C = new Float64Array( 10 );
	var WORK = new Float64Array( 40 );
	var info = zunmqr( 'L', 'N', 4, 4, 0, A, 1, 4, 0, TAU, 1, 0, C, 1, 4, 0, WORK, 1, 0, 20 );
	assertClose( info, tc.info, 1e-14, 'info' );
});

test( 'zunmqr: left, no transpose, rectangular C (4x2)', function t() {
	var tc = findCase( 'left_notrans_rect' );
	var qr = qr4x3();
	var LDC = 6;
	var C = new Float64Array( 2 * LDC * 6 );
	// C = [1+1i 0+2i; 3+0i 1-1i; -1+1i 4+0i; 2+0i 0+3i]
	C[0]=1; C[1]=1; C[2]=3; C[3]=0; C[4]=-1; C[5]=1; C[6]=2; C[7]=0;
	C[2*LDC]=0; C[2*LDC+1]=2; C[2*LDC+2]=1; C[2*LDC+3]=-1; C[2*LDC+4]=4; C[2*LDC+5]=0; C[2*LDC+6]=0; C[2*LDC+7]=3;

	var WORK = new Float64Array( 400 );
	var info = zunmqr( 'L', 'N', 4, 2, 3, qr.A, 1, qr.LDA, 0, qr.TAU, 1, 0, C, 1, LDC, 0, WORK, 1, 0, 200 );
	assertClose( info, tc.info, 1e-14, 'info' );
	assertArrayClose( Array.from( C.subarray( 0, tc.c.length ) ), tc.c, 1e-10, 'c' );
});
