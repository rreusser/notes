'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var zlaqp2 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zlaqp2.jsonl' ), 'utf8' ).trim().split( '\n' );
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
* A is col-major with strideA1=1, strideA2=LDA (complex elements).
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

test( 'zlaqp2: basic 3x3 matrix', function t() {
	var tc = findCase( 'basic_3x3' );
	var LDA = 6;
	var A = new Float64Array( 2 * LDA * 6 );
	// A = [1+0i 2+1i 3+0i; 0+1i 1+0i 2+1i; 1+1i 0+0i 1+0i] col-major
	A[0]=1; A[1]=0; A[2]=0; A[3]=1; A[4]=1; A[5]=1;
	A[2*LDA]=2; A[2*LDA+1]=1; A[2*LDA+2]=1; A[2*LDA+3]=0; A[2*LDA+4]=0; A[2*LDA+5]=0;
	A[4*LDA]=3; A[4*LDA+1]=0; A[4*LDA+2]=2; A[4*LDA+3]=1; A[4*LDA+4]=1; A[4*LDA+5]=0;

	var JPVT = new Int32Array( [ 1, 2, 3 ] );
	var TAU = new Float64Array( 6 );
	var VN1 = colNorms( 3, 3, 0, A, LDA, 0 );
	var VN2 = new Float64Array( VN1 );
	var WORK = new Float64Array( 40 );

	zlaqp2( 3, 3, 0, A, 1, LDA, 0, JPVT, 1, 0, TAU, 1, 0, VN1, 1, 0, VN2, 1, 0, WORK, 1, 0 );

	assertArrayClose( Array.from( A.subarray( 0, 36 ) ), tc.a, 1e-10, 'a' );
	assertArrayClose( Array.from( TAU ), tc.tau, 1e-10, 'tau' );
	assert.deepStrictEqual( Array.from( JPVT ), tc.jpvt, 'jpvt' );
});

test( 'zlaqp2: 4x3 matrix', function t() {
	var tc = findCase( 'rect_4x3' );
	var LDA = 6;
	var A = new Float64Array( 2 * LDA * 6 );
	// A = [1+0i 0+2i 3+1i; 2+1i 1+0i 0+0i; 0+0i 3+1i 1+0i; 1+1i 2+0i 2+1i]
	A[0]=1; A[1]=0; A[2]=2; A[3]=1; A[4]=0; A[5]=0; A[6]=1; A[7]=1;
	A[2*LDA]=0; A[2*LDA+1]=2; A[2*LDA+2]=1; A[2*LDA+3]=0; A[2*LDA+4]=3; A[2*LDA+5]=1; A[2*LDA+6]=2; A[2*LDA+7]=0;
	A[4*LDA]=3; A[4*LDA+1]=1; A[4*LDA+2]=0; A[4*LDA+3]=0; A[4*LDA+4]=1; A[4*LDA+5]=0; A[4*LDA+6]=2; A[4*LDA+7]=1;

	var JPVT = new Int32Array( [ 1, 2, 3 ] );
	var TAU = new Float64Array( 6 );
	var VN1 = colNorms( 4, 3, 0, A, LDA, 0 );
	var VN2 = new Float64Array( VN1 );
	var WORK = new Float64Array( 40 );

	zlaqp2( 4, 3, 0, A, 1, LDA, 0, JPVT, 1, 0, TAU, 1, 0, VN1, 1, 0, VN2, 1, 0, WORK, 1, 0 );

	assertArrayClose( Array.from( A.subarray( 0, 48 ) ), tc.a, 1e-10, 'a' );
	assertArrayClose( Array.from( TAU ), tc.tau, 1e-10, 'tau' );
	assert.deepStrictEqual( Array.from( JPVT ), tc.jpvt, 'jpvt' );
});

test( 'zlaqp2: with offset=1', function t() {
	var tc = findCase( 'offset_1' );
	var LDA = 6;
	var A = new Float64Array( 2 * LDA * 6 );
	// A = [5+0i 1+0i 2+0i; 0+0i 1+1i 2+0i; 0+0i 3+0i 0+1i; 0+0i 2+1i 1+0i]
	A[0]=5; A[1]=0; A[2]=0; A[3]=0; A[4]=0; A[5]=0; A[6]=0; A[7]=0;
	A[2*LDA]=1; A[2*LDA+1]=0; A[2*LDA+2]=1; A[2*LDA+3]=1; A[2*LDA+4]=3; A[2*LDA+5]=0; A[2*LDA+6]=2; A[2*LDA+7]=1;
	A[4*LDA]=2; A[4*LDA+1]=0; A[4*LDA+2]=2; A[4*LDA+3]=0; A[4*LDA+4]=0; A[4*LDA+5]=1; A[4*LDA+6]=1; A[4*LDA+7]=0;

	var JPVT = new Int32Array( [ 1, 2, 3 ] );
	var TAU = new Float64Array( 6 );
	var VN1 = colNorms( 4, 3, 1, A, LDA, 0 );
	var VN2 = new Float64Array( VN1 );
	var WORK = new Float64Array( 40 );

	zlaqp2( 4, 3, 1, A, 1, LDA, 0, JPVT, 1, 0, TAU, 1, 0, VN1, 1, 0, VN2, 1, 0, WORK, 1, 0 );

	assertArrayClose( Array.from( A.subarray( 0, 48 ) ), tc.a, 1e-10, 'a' );
	assertArrayClose( Array.from( TAU ), tc.tau, 1e-10, 'tau' );
	assert.deepStrictEqual( Array.from( JPVT ), tc.jpvt, 'jpvt' );
});

test( 'zlaqp2: 1x1 matrix', function t() {
	var tc = findCase( 'one_by_one' );
	var LDA = 6;
	var A = new Float64Array( 2 * LDA * 6 );
	A[0] = 3; A[1] = 4;

	var JPVT = new Int32Array( [ 1 ] );
	var TAU = new Float64Array( 2 );
	var VN1 = new Float64Array( [ 5.0 ] );
	var VN2 = new Float64Array( [ 5.0 ] );
	var WORK = new Float64Array( 40 );

	zlaqp2( 1, 1, 0, A, 1, LDA, 0, JPVT, 1, 0, TAU, 1, 0, VN1, 1, 0, VN2, 1, 0, WORK, 1, 0 );

	assertArrayClose( Array.from( A.subarray( 0, 2 ) ), tc.a, 1e-10, 'a' );
	assertArrayClose( Array.from( TAU ), tc.tau, 1e-10, 'tau' );
	assert.deepStrictEqual( Array.from( JPVT ), tc.jpvt, 'jpvt' );
});

test( 'zlaqp2: N=0 quick return', function t() {
	var A = new Float64Array( 20 );
	var JPVT = new Int32Array( 1 );
	var TAU = new Float64Array( 2 );
	var VN1 = new Float64Array( 1 );
	var VN2 = new Float64Array( 1 );
	var WORK = new Float64Array( 10 );

	zlaqp2( 3, 0, 0, A, 1, 3, 0, JPVT, 1, 0, TAU, 1, 0, VN1, 1, 0, VN2, 1, 0, WORK, 1, 0 );
	assert.ok( true, 'no crash' );
});
