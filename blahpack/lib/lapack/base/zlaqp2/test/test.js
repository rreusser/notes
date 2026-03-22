'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
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
	var buf = reinterpret( A, 0 );
	var vn = new Float64Array( N );
	var re;
	var im;
	var s;
	var i;
	var j;
	for ( j = 0; j < N; j++ ) {
		s = 0.0;
		for ( i = startRow; i < M; i++ ) {
			re = buf[ offsetA + 2 * ( i + j * LDA ) ];
			im = buf[ offsetA + 2 * ( i + j * LDA ) + 1 ];
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
	var A = new Complex128Array( LDA * 6 );
	var Av = reinterpret( A, 0 );
	// A = [1+0i 2+1i 3+0i; 0+1i 1+0i 2+1i; 1+1i 0+0i 1+0i] col-major
	Av[0]=1; Av[1]=0; Av[2]=0; Av[3]=1; Av[4]=1; Av[5]=1;
	Av[2*LDA]=2; Av[2*LDA+1]=1; Av[2*LDA+2]=1; Av[2*LDA+3]=0; Av[2*LDA+4]=0; Av[2*LDA+5]=0;
	Av[4*LDA]=3; Av[4*LDA+1]=0; Av[4*LDA+2]=2; Av[4*LDA+3]=1; Av[4*LDA+4]=1; Av[4*LDA+5]=0;

	var JPVT = new Int32Array( [ 1, 2, 3 ] );
	var TAU = new Complex128Array( 3 );
	var VN1 = colNorms( 3, 3, 0, A, LDA, 0 );
	var VN2 = new Float64Array( VN1 );
	var WORK = new Complex128Array( 20 );

	zlaqp2( 3, 3, 0, A, 1, LDA, 0, JPVT, 1, 0, TAU, 1, 0, VN1, 1, 0, VN2, 1, 0, WORK, 1, 0 );

	assertArrayClose( Array.from( Av.subarray( 0, 36 ) ), tc.a, 1e-10, 'a' );
	assertArrayClose( Array.from( reinterpret( TAU, 0 ) ), tc.tau, 1e-10, 'tau' );
	assert.deepStrictEqual( Array.from( JPVT ), tc.jpvt, 'jpvt' );
});

test( 'zlaqp2: 4x3 matrix', function t() {
	var tc = findCase( 'rect_4x3' );
	var LDA = 6;
	var A = new Complex128Array( LDA * 6 );
	var Av = reinterpret( A, 0 );
	// A = [1+0i 0+2i 3+1i; 2+1i 1+0i 0+0i; 0+0i 3+1i 1+0i; 1+1i 2+0i 2+1i]
	Av[0]=1; Av[1]=0; Av[2]=2; Av[3]=1; Av[4]=0; Av[5]=0; Av[6]=1; Av[7]=1;
	Av[2*LDA]=0; Av[2*LDA+1]=2; Av[2*LDA+2]=1; Av[2*LDA+3]=0; Av[2*LDA+4]=3; Av[2*LDA+5]=1; Av[2*LDA+6]=2; Av[2*LDA+7]=0;
	Av[4*LDA]=3; Av[4*LDA+1]=1; Av[4*LDA+2]=0; Av[4*LDA+3]=0; Av[4*LDA+4]=1; Av[4*LDA+5]=0; Av[4*LDA+6]=2; Av[4*LDA+7]=1;

	var JPVT = new Int32Array( [ 1, 2, 3 ] );
	var TAU = new Complex128Array( 3 );
	var VN1 = colNorms( 4, 3, 0, A, LDA, 0 );
	var VN2 = new Float64Array( VN1 );
	var WORK = new Complex128Array( 20 );

	zlaqp2( 4, 3, 0, A, 1, LDA, 0, JPVT, 1, 0, TAU, 1, 0, VN1, 1, 0, VN2, 1, 0, WORK, 1, 0 );

	assertArrayClose( Array.from( Av.subarray( 0, 48 ) ), tc.a, 1e-10, 'a' );
	assertArrayClose( Array.from( reinterpret( TAU, 0 ) ), tc.tau, 1e-10, 'tau' );
	assert.deepStrictEqual( Array.from( JPVT ), tc.jpvt, 'jpvt' );
});

test( 'zlaqp2: with offset=1', function t() {
	var tc = findCase( 'offset_1' );
	var LDA = 6;
	var A = new Complex128Array( LDA * 6 );
	var Av = reinterpret( A, 0 );
	// A = [5+0i 1+0i 2+0i; 0+0i 1+1i 2+0i; 0+0i 3+0i 0+1i; 0+0i 2+1i 1+0i]
	Av[0]=5; Av[1]=0; Av[2]=0; Av[3]=0; Av[4]=0; Av[5]=0; Av[6]=0; Av[7]=0;
	Av[2*LDA]=1; Av[2*LDA+1]=0; Av[2*LDA+2]=1; Av[2*LDA+3]=1; Av[2*LDA+4]=3; Av[2*LDA+5]=0; Av[2*LDA+6]=2; Av[2*LDA+7]=1;
	Av[4*LDA]=2; Av[4*LDA+1]=0; Av[4*LDA+2]=2; Av[4*LDA+3]=0; Av[4*LDA+4]=0; Av[4*LDA+5]=1; Av[4*LDA+6]=1; Av[4*LDA+7]=0;

	var JPVT = new Int32Array( [ 1, 2, 3 ] );
	var TAU = new Complex128Array( 3 );
	var VN1 = colNorms( 4, 3, 1, A, LDA, 0 );
	var VN2 = new Float64Array( VN1 );
	var WORK = new Complex128Array( 20 );

	zlaqp2( 4, 3, 1, A, 1, LDA, 0, JPVT, 1, 0, TAU, 1, 0, VN1, 1, 0, VN2, 1, 0, WORK, 1, 0 );

	assertArrayClose( Array.from( Av.subarray( 0, 48 ) ), tc.a, 1e-10, 'a' );
	assertArrayClose( Array.from( reinterpret( TAU, 0 ) ), tc.tau, 1e-10, 'tau' );
	assert.deepStrictEqual( Array.from( JPVT ), tc.jpvt, 'jpvt' );
});

test( 'zlaqp2: 1x1 matrix', function t() {
	var tc = findCase( 'one_by_one' );
	var LDA = 6;
	var A = new Complex128Array( LDA * 6 );
	var Av = reinterpret( A, 0 );
	Av[0] = 3; Av[1] = 4;

	var JPVT = new Int32Array( [ 1 ] );
	var TAU = new Complex128Array( 1 );
	var VN1 = new Float64Array( [ 5.0 ] );
	var VN2 = new Float64Array( [ 5.0 ] );
	var WORK = new Complex128Array( 20 );

	zlaqp2( 1, 1, 0, A, 1, LDA, 0, JPVT, 1, 0, TAU, 1, 0, VN1, 1, 0, VN2, 1, 0, WORK, 1, 0 );

	assertArrayClose( Array.from( Av.subarray( 0, 2 ) ), tc.a, 1e-10, 'a' );
	assertArrayClose( Array.from( reinterpret( TAU, 0 ) ), tc.tau, 1e-10, 'tau' );
	assert.deepStrictEqual( Array.from( JPVT ), tc.jpvt, 'jpvt' );
});

test( 'zlaqp2: collinear columns (norm recomputation)', function t() {
	var tc = findCase( 'collinear_norm_recomp' );
	var LDA = 6;
	var A = new Complex128Array( LDA * 6 );
	var Av = reinterpret( A, 0 );

	// Nearly collinear columns (same as Fortran test)
	Av[0]=1; Av[1]=0; Av[2]=2; Av[3]=0; Av[4]=3; Av[5]=0; Av[6]=4; Av[7]=0; Av[8]=5; Av[9]=0; Av[10]=6; Av[11]=0;
	Av[2*LDA]=1; Av[2*LDA+1]=1e-10; Av[2*LDA+2]=2; Av[2*LDA+3]=1e-10; Av[2*LDA+4]=3; Av[2*LDA+5]=1e-10; Av[2*LDA+6]=4; Av[2*LDA+7]=1e-10; Av[2*LDA+8]=5; Av[2*LDA+9]=1e-10; Av[2*LDA+10]=6; Av[2*LDA+11]=1e-10;
	Av[4*LDA]=1; Av[4*LDA+1]=0; Av[4*LDA+2]=2; Av[4*LDA+3]=0; Av[4*LDA+4]=3; Av[4*LDA+5]=0; Av[4*LDA+6]=4; Av[4*LDA+7]=0; Av[4*LDA+8]=5; Av[4*LDA+9]=0; Av[4*LDA+10]=6.0000000001; Av[4*LDA+11]=0;

	var JPVT = new Int32Array( [ 1, 2, 3 ] );
	var TAU = new Complex128Array( 3 );
	var VN1 = colNorms( 6, 3, 0, A, LDA, 0 );
	var VN2 = new Float64Array( VN1 );
	var WORK = new Complex128Array( 20 );

	zlaqp2( 6, 3, 0, A, 1, LDA, 0, JPVT, 1, 0, TAU, 1, 0, VN1, 1, 0, VN2, 1, 0, WORK, 1, 0 );

	assertArrayClose( Array.from( Av.subarray( 0, 72 ) ), tc.a, 1e-10, 'a' );
	assertArrayClose( Array.from( reinterpret( TAU, 0 ) ), tc.tau, 1e-10, 'tau' );
	assert.deepStrictEqual( Array.from( JPVT ), tc.jpvt, 'jpvt' );
});

test( 'zlaqp2: square collinear (offpi == M-1 norm zero path)', function t() {
	var tc = findCase( 'square_collinear' );
	var LDA = 6;
	var A = new Complex128Array( LDA * 6 );
	var Av = reinterpret( A, 0 );

	Av[0]=1; Av[1]=0; Av[2]=2; Av[3]=0; Av[4]=3; Av[5]=0;
	Av[2*LDA]=1; Av[2*LDA+1]=1e-14; Av[2*LDA+2]=2; Av[2*LDA+3]=1e-14; Av[2*LDA+4]=3; Av[2*LDA+5]=1e-14;
	Av[4*LDA]=1; Av[4*LDA+1]=0; Av[4*LDA+2]=2; Av[4*LDA+3]=1e-14; Av[4*LDA+4]=3; Av[4*LDA+5]=0;

	var JPVT = new Int32Array( [ 1, 2, 3 ] );
	var TAU = new Complex128Array( 3 );
	var VN1 = colNorms( 3, 3, 0, A, LDA, 0 );
	var VN2 = new Float64Array( VN1 );
	var WORK = new Complex128Array( 20 );

	zlaqp2( 3, 3, 0, A, 1, LDA, 0, JPVT, 1, 0, TAU, 1, 0, VN1, 1, 0, VN2, 1, 0, WORK, 1, 0 );

	assertArrayClose( Array.from( Av.subarray( 0, 36 ) ), tc.a, 1e-10, 'a' );
	assertArrayClose( Array.from( reinterpret( TAU, 0 ) ), tc.tau, 1e-10, 'tau' );
	assert.deepStrictEqual( Array.from( JPVT ), tc.jpvt, 'jpvt' );
});

test( 'zlaqp2: N=0 quick return', function t() {
	var A = new Complex128Array( 10 );
	var JPVT = new Int32Array( 1 );
	var TAU = new Complex128Array( 1 );
	var VN1 = new Float64Array( 1 );
	var VN2 = new Float64Array( 1 );
	var WORK = new Complex128Array( 5 );

	zlaqp2( 3, 0, 0, A, 1, 3, 0, JPVT, 1, 0, TAU, 1, 0, VN1, 1, 0, VN2, 1, 0, WORK, 1, 0 );
	assert.ok( true, 'no crash' );
});
