'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var zlaqps = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zlaqps.jsonl' ), 'utf8' ).trim().split( '\n' );
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
* Compute column norms of complex matrix A(startRow:M-1, 0:N-1).
*/
function colNorms( M, N, startRow, A, LDA, offsetA ) {
	var vn = new Float64Array( N );
	var re;
	var im;
	var s;
	var i;
	var j;
	for ( j = 0; j < N; j++ ) {
		s = 0.0;
		for ( i = startRow; i < M; i++ ) {
			re = A[ offsetA + 2 * ( i + j * LDA ) ];
			im = A[ offsetA + 2 * ( i + j * LDA ) + 1 ];
			s += re * re + im * im;
		}
		vn[ j ] = Math.sqrt( s );
	}
	return vn;
}


// TESTS //

test( 'zlaqps: basic 4x3 NB=2', function t() {
	var tc = findCase( 'basic_4x3_nb2' );
	var LDA = 8;
	var LDF = 8;
	var M = 4;
	var N = 3;
	var NB = 2;
	var A = new Float64Array( 2 * LDA * N );

	// A = [1+0i 0+2i 3+1i; 2+1i 1+0i 0+0i; 0+0i 3+1i 1+0i; 1+1i 2+0i 2+1i]
	A[0]=1; A[1]=0; A[2]=2; A[3]=1; A[4]=0; A[5]=0; A[6]=1; A[7]=1;
	A[2*LDA]=0; A[2*LDA+1]=2; A[2*LDA+2]=1; A[2*LDA+3]=0; A[2*LDA+4]=3; A[2*LDA+5]=1; A[2*LDA+6]=2; A[2*LDA+7]=0;
	A[4*LDA]=3; A[4*LDA+1]=1; A[4*LDA+2]=0; A[4*LDA+3]=0; A[4*LDA+4]=1; A[4*LDA+5]=0; A[4*LDA+6]=2; A[4*LDA+7]=1;

	var JPVT = new Int32Array( [ 1, 2, 3 ] );
	var TAU = new Float64Array( 2 * N );
	var VN1 = colNorms( M, N, 0, A, LDA, 0 );
	var VN2 = new Float64Array( VN1 );
	var AUXV = new Float64Array( 2 * NB );
	var F = new Float64Array( 2 * LDF * N );
	var WORK = new Float64Array( 40 );

	var kb = zlaqps( M, N, 0, NB, A, 1, LDA, 0, JPVT, 1, 0, TAU, 1, 0, VN1, 1, 0, VN2, 1, 0, AUXV, 1, 0, F, 1, LDF, 0 );

	assert.equal( kb, tc.kb, 'kb' );
	assertArrayClose( Array.from( A.subarray( 0, tc.a.length ) ), tc.a, 1e-10, 'a' );
	assertArrayClose( Array.from( TAU.subarray( 0, tc.tau.length ) ), tc.tau, 1e-10, 'tau' );
	assert.deepStrictEqual( Array.from( JPVT ), tc.jpvt, 'jpvt' );
});

test( 'zlaqps: 6x4 NB=3', function t() {
	var tc = findCase( 'rect_6x4_nb3' );
	var LDA = 8;
	var LDF = 8;
	var M = 6;
	var N = 4;
	var NB = 3;
	var A = new Float64Array( 2 * LDA * N );

	A[0]=2; A[1]=0; A[2]=1; A[3]=1; A[4]=0; A[5]=1; A[6]=3; A[7]=0; A[8]=1; A[9]=0; A[10]=0; A[11]=2;
	A[2*LDA]=1; A[2*LDA+1]=0; A[2*LDA+2]=0; A[2*LDA+3]=0; A[2*LDA+4]=2; A[2*LDA+5]=1; A[2*LDA+6]=1; A[2*LDA+7]=1; A[2*LDA+8]=0; A[2*LDA+9]=0; A[2*LDA+10]=3; A[2*LDA+11]=0;
	A[4*LDA]=0; A[4*LDA+1]=1; A[4*LDA+2]=3; A[4*LDA+3]=0; A[4*LDA+4]=1; A[4*LDA+5]=0; A[4*LDA+6]=0; A[4*LDA+7]=2; A[4*LDA+8]=2; A[4*LDA+9]=1; A[4*LDA+10]=1; A[4*LDA+11]=0;
	A[6*LDA]=1; A[6*LDA+1]=1; A[6*LDA+2]=2; A[6*LDA+3]=0; A[6*LDA+4]=0; A[6*LDA+5]=0; A[6*LDA+6]=1; A[6*LDA+7]=0; A[6*LDA+8]=3; A[6*LDA+9]=1; A[6*LDA+10]=0; A[6*LDA+11]=1;

	var JPVT = new Int32Array( [ 1, 2, 3, 4 ] );
	var TAU = new Float64Array( 2 * N );
	var VN1 = colNorms( M, N, 0, A, LDA, 0 );
	var VN2 = new Float64Array( VN1 );
	var AUXV = new Float64Array( 2 * NB );
	var F = new Float64Array( 2 * LDF * N );

	var kb = zlaqps( M, N, 0, NB, A, 1, LDA, 0, JPVT, 1, 0, TAU, 1, 0, VN1, 1, 0, VN2, 1, 0, AUXV, 1, 0, F, 1, LDF, 0 );

	assert.equal( kb, tc.kb, 'kb' );
	assertArrayClose( Array.from( A.subarray( 0, tc.a.length ) ), tc.a, 1e-10, 'a' );
	assertArrayClose( Array.from( TAU.subarray( 0, tc.tau.length ) ), tc.tau, 1e-10, 'tau' );
	assert.deepStrictEqual( Array.from( JPVT ), tc.jpvt, 'jpvt' );
});

test( 'zlaqps: NB=1', function t() {
	var tc = findCase( 'nb_1' );
	var LDA = 8;
	var LDF = 8;
	var M = 3;
	var N = 3;
	var NB = 1;
	var A = new Float64Array( 2 * LDA * N );

	A[0]=1; A[1]=0; A[2]=2; A[3]=0; A[4]=3; A[5]=0;
	A[2*LDA]=4; A[2*LDA+1]=1; A[2*LDA+2]=5; A[2*LDA+3]=1; A[2*LDA+4]=6; A[2*LDA+5]=1;
	A[4*LDA]=0; A[4*LDA+1]=1; A[4*LDA+2]=1; A[4*LDA+3]=0; A[4*LDA+4]=2; A[4*LDA+5]=1;

	var JPVT = new Int32Array( [ 1, 2, 3 ] );
	var TAU = new Float64Array( 2 * N );
	var VN1 = colNorms( M, N, 0, A, LDA, 0 );
	var VN2 = new Float64Array( VN1 );
	var AUXV = new Float64Array( 2 * NB );
	var F = new Float64Array( 2 * LDF * N );

	var kb = zlaqps( M, N, 0, NB, A, 1, LDA, 0, JPVT, 1, 0, TAU, 1, 0, VN1, 1, 0, VN2, 1, 0, AUXV, 1, 0, F, 1, LDF, 0 );

	assert.equal( kb, tc.kb, 'kb' );
	assertArrayClose( Array.from( A.subarray( 0, tc.a.length ) ), tc.a, 1e-10, 'a' );
	// Only compare first kb tau values (the rest may have residual data)
	assertArrayClose( Array.from( TAU.subarray( 0, 2 * kb ) ), tc.tau.slice( 0, 2 * kb ), 1e-10, 'tau' );
	assert.deepStrictEqual( Array.from( JPVT ), tc.jpvt, 'jpvt' );
});

test( 'zlaqps: offset=1', function t() {
	var tc = findCase( 'offset_1' );
	var LDA = 8;
	var LDF = 8;
	var M = 4;
	var N = 2;
	var NB = 2;
	var A = new Float64Array( 2 * LDA * N );

	A[0]=5; A[1]=0; A[2]=0; A[3]=0; A[4]=0; A[5]=0; A[6]=0; A[7]=0;
	A[2*LDA]=1; A[2*LDA+1]=0; A[2*LDA+2]=2; A[2*LDA+3]=1; A[2*LDA+4]=1; A[2*LDA+5]=0; A[2*LDA+6]=3; A[2*LDA+7]=0;

	var JPVT = new Int32Array( [ 1, 2 ] );
	var TAU = new Float64Array( 2 * N );
	var VN1 = colNorms( M, N, 1, A, LDA, 0 );
	var VN2 = new Float64Array( VN1 );
	var AUXV = new Float64Array( 2 * NB );
	var F = new Float64Array( 2 * LDF * N );

	var kb = zlaqps( M, N, 1, NB, A, 1, LDA, 0, JPVT, 1, 0, TAU, 1, 0, VN1, 1, 0, VN2, 1, 0, AUXV, 1, 0, F, 1, LDF, 0 );

	assert.equal( kb, tc.kb, 'kb' );
	assertArrayClose( Array.from( A.subarray( 0, tc.a.length ) ), tc.a, 1e-10, 'a' );
	assertArrayClose( Array.from( TAU.subarray( 0, tc.tau.length ) ), tc.tau, 1e-10, 'tau' );
	assert.deepStrictEqual( Array.from( JPVT ), tc.jpvt, 'jpvt' );
});
