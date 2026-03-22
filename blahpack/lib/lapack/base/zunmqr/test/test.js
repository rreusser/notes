'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
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
	var A = new Complex128Array( LDA * 6 );
	var Av = reinterpret( A, 0 );
	Av[0]=1; Av[1]=0; Av[2]=2; Av[3]=1; Av[4]=0; Av[5]=0; Av[6]=1; Av[7]=1;
	Av[2*LDA]=0; Av[2*LDA+1]=2; Av[2*LDA+2]=1; Av[2*LDA+3]=0; Av[2*LDA+4]=3; Av[2*LDA+5]=1; Av[2*LDA+6]=2; Av[2*LDA+7]=0;
	Av[4*LDA]=3; Av[4*LDA+1]=1; Av[4*LDA+2]=0; Av[4*LDA+3]=0; Av[4*LDA+4]=1; Av[4*LDA+5]=0; Av[4*LDA+6]=2; Av[4*LDA+7]=1;
	var TAU = new Complex128Array( 3 );
	var WORK = new Complex128Array( 20 );
	zgeqr2( 4, 3, A, 1, LDA, 0, TAU, 1, 0, WORK, 1, 0 );
	return { A: A, TAU: TAU, LDA: LDA };
}

/**
* Create 4x4 identity in a 6x6 container (LDC=6, complex).
*/
function eye4in6() {
	var LDC = 6;
	var C = new Complex128Array( LDC * 6 );
	var Cv = reinterpret( C, 0 );
	Cv[ 2 * ( 0 + 0 * LDC ) ] = 1.0;
	Cv[ 2 * ( 1 + 1 * LDC ) ] = 1.0;
	Cv[ 2 * ( 2 + 2 * LDC ) ] = 1.0;
	Cv[ 2 * ( 3 + 3 * LDC ) ] = 1.0;
	return C;
}


// TESTS //

test( 'zunmqr: left, no transpose (Q*I)', function t() {
	var tc = findCase( 'left_notrans_4x4' );
	var qr = qr4x3();
	var LDC = 6;
	var C = eye4in6();
	var WORK = new Complex128Array( 200 );
	var info = zunmqr( 'L', 'N', 4, 4, 3, qr.A, 1, qr.LDA, 0, qr.TAU, 1, 0, C, 1, LDC, 0, WORK, 1, 0, 200 );
	assertClose( info, tc.info, 1e-14, 'info' );
	var Cv = reinterpret( C, 0 );
	assertArrayClose( Array.from( Cv.subarray( 0, tc.c.length ) ), tc.c, 1e-10, 'c' );
});

test( 'zunmqr: left, conjugate transpose (Q^H*I)', function t() {
	var tc = findCase( 'left_conjtrans_4x4' );
	var qr = qr4x3();
	var LDC = 6;
	var C = eye4in6();
	var WORK = new Complex128Array( 200 );
	var info = zunmqr( 'L', 'C', 4, 4, 3, qr.A, 1, qr.LDA, 0, qr.TAU, 1, 0, C, 1, LDC, 0, WORK, 1, 0, 200 );
	assertClose( info, tc.info, 1e-14, 'info' );
	var Cv = reinterpret( C, 0 );
	assertArrayClose( Array.from( Cv.subarray( 0, tc.c.length ) ), tc.c, 1e-10, 'c' );
});

test( 'zunmqr: right, no transpose (I*Q)', function t() {
	var tc = findCase( 'right_notrans_4x4' );
	var qr = qr4x3();
	var LDC = 6;
	var C = eye4in6();
	var WORK = new Complex128Array( 200 );
	var info = zunmqr( 'R', 'N', 4, 4, 3, qr.A, 1, qr.LDA, 0, qr.TAU, 1, 0, C, 1, LDC, 0, WORK, 1, 0, 200 );
	assertClose( info, tc.info, 1e-14, 'info' );
	var Cv = reinterpret( C, 0 );
	assertArrayClose( Array.from( Cv.subarray( 0, tc.c.length ) ), tc.c, 1e-10, 'c' );
});

test( 'zunmqr: M=0 quick return', function t() {
	var tc = findCase( 'm_zero' );
	var A = new Complex128Array( 5 );
	var TAU = new Complex128Array( 2 );
	var C = new Complex128Array( 5 );
	var WORK = new Complex128Array( 20 );
	var info = zunmqr( 'L', 'N', 0, 4, 0, A, 1, 1, 0, TAU, 1, 0, C, 1, 1, 0, WORK, 1, 0, 20 );
	assertClose( info, tc.info, 1e-14, 'info' );
});

test( 'zunmqr: N=0 quick return', function t() {
	var tc = findCase( 'n_zero' );
	var A = new Complex128Array( 5 );
	var TAU = new Complex128Array( 2 );
	var C = new Complex128Array( 5 );
	var WORK = new Complex128Array( 20 );
	var info = zunmqr( 'L', 'N', 4, 0, 0, A, 1, 4, 0, TAU, 1, 0, C, 1, 4, 0, WORK, 1, 0, 20 );
	assertClose( info, tc.info, 1e-14, 'info' );
});

test( 'zunmqr: K=0 quick return', function t() {
	var tc = findCase( 'k_zero' );
	var A = new Complex128Array( 5 );
	var TAU = new Complex128Array( 2 );
	var C = new Complex128Array( 5 );
	var WORK = new Complex128Array( 20 );
	var info = zunmqr( 'L', 'N', 4, 4, 0, A, 1, 4, 0, TAU, 1, 0, C, 1, 4, 0, WORK, 1, 0, 20 );
	assertClose( info, tc.info, 1e-14, 'info' );
});

test( 'zunmqr: left, no transpose, rectangular C (4x2)', function t() {
	var tc = findCase( 'left_notrans_rect' );
	var qr = qr4x3();
	var LDC = 6;
	var C = new Complex128Array( LDC * 6 );
	var Cv = reinterpret( C, 0 );
	// C = [1+1i 0+2i; 3+0i 1-1i; -1+1i 4+0i; 2+0i 0+3i]
	Cv[0]=1; Cv[1]=1; Cv[2]=3; Cv[3]=0; Cv[4]=-1; Cv[5]=1; Cv[6]=2; Cv[7]=0;
	Cv[2*LDC]=0; Cv[2*LDC+1]=2; Cv[2*LDC+2]=1; Cv[2*LDC+3]=-1; Cv[2*LDC+4]=4; Cv[2*LDC+5]=0; Cv[2*LDC+6]=0; Cv[2*LDC+7]=3;

	var WORK = new Complex128Array( 200 );
	var info = zunmqr( 'L', 'N', 4, 2, 3, qr.A, 1, qr.LDA, 0, qr.TAU, 1, 0, C, 1, LDC, 0, WORK, 1, 0, 200 );
	assertClose( info, tc.info, 1e-14, 'info' );
	assertArrayClose( Array.from( Cv.subarray( 0, tc.c.length ) ), tc.c, 1e-10, 'c' );
});
