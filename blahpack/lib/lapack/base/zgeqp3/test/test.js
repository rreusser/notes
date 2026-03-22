'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
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
	var A = new Complex128Array( LDA * LDA );
	var Av = reinterpret( A, 0 );
	// A = [1 0+2i 3+i; 2+i 1 0; 0 3+i 1; 1+i 2 2+i]
	Av[0]=1; Av[1]=0; Av[2]=2; Av[3]=1; Av[4]=0; Av[5]=0; Av[6]=1; Av[7]=1;
	Av[2*LDA]=0; Av[2*LDA+1]=2; Av[2*LDA+2]=1; Av[2*LDA+3]=0; Av[2*LDA+4]=3; Av[2*LDA+5]=1; Av[2*LDA+6]=2; Av[2*LDA+7]=0;
	Av[4*LDA]=3; Av[4*LDA+1]=1; Av[4*LDA+2]=0; Av[4*LDA+3]=0; Av[4*LDA+4]=1; Av[4*LDA+5]=0; Av[4*LDA+6]=2; Av[4*LDA+7]=1;

	var JPVT = new Int32Array( 3 );
	var TAU = new Complex128Array( 3 );
	var WORK = new Complex128Array( 200 );
	var RWORK = new Float64Array( 2 * LDA );

	var info = zgeqp3( 4, 3, A, 1, LDA, 0, JPVT, 1, 0, TAU, 1, 0, WORK, 1, 0, 200, RWORK, 1, 0 );

	assertClose( info, tc.info, 1e-14, 'info' );
	assertArrayClose( Array.from( Av.subarray( 0, tc.a.length ) ), tc.a, 1e-10, 'a' );
	assertArrayClose( Array.from( reinterpret( TAU, 0 ) ), tc.tau, 1e-10, 'tau' );
	assert.deepStrictEqual( Array.from( JPVT ), tc.jpvt, 'jpvt' );
});

test( 'zgeqp3: 3x4 matrix', function t() {
	var tc = findCase( 'rect_3x4' );
	var A = new Complex128Array( LDA * LDA );
	var Av = reinterpret( A, 0 );
	Av[0]=1; Av[1]=0; Av[2]=0; Av[3]=1; Av[4]=2; Av[5]=0;
	Av[2*LDA]=3; Av[2*LDA+1]=1; Av[2*LDA+2]=1; Av[2*LDA+3]=0; Av[2*LDA+4]=0; Av[2*LDA+5]=1;
	Av[4*LDA]=0; Av[4*LDA+1]=2; Av[4*LDA+2]=2; Av[4*LDA+3]=1; Av[4*LDA+4]=1; Av[4*LDA+5]=0;
	Av[6*LDA]=1; Av[6*LDA+1]=1; Av[6*LDA+2]=0; Av[6*LDA+3]=0; Av[6*LDA+4]=3; Av[6*LDA+5]=0;

	var JPVT = new Int32Array( 4 );
	var TAU = new Complex128Array( 3 );
	var WORK = new Complex128Array( 200 );
	var RWORK = new Float64Array( 2 * LDA );

	var info = zgeqp3( 3, 4, A, 1, LDA, 0, JPVT, 1, 0, TAU, 1, 0, WORK, 1, 0, 200, RWORK, 1, 0 );

	assertClose( info, tc.info, 1e-14, 'info' );
	assertArrayClose( Array.from( Av.subarray( 0, tc.a.length ) ), tc.a, 1e-10, 'a' );
	assertArrayClose( Array.from( reinterpret( TAU, 0 ) ), tc.tau, 1e-10, 'tau' );
	assert.deepStrictEqual( Array.from( JPVT ), tc.jpvt, 'jpvt' );
});

test( 'zgeqp3: rank-deficient 3x3', function t() {
	var A = new Complex128Array( LDA * LDA );
	var Av = reinterpret( A, 0 );
	// col 3 = col 1 + col 2, so rank = 1
	Av[0]=1; Av[1]=0; Av[2]=2; Av[3]=0; Av[4]=3; Av[5]=0;
	Av[2*LDA]=0; Av[2*LDA+1]=1; Av[2*LDA+2]=0; Av[2*LDA+3]=2; Av[2*LDA+4]=0; Av[2*LDA+5]=3;
	Av[4*LDA]=1; Av[4*LDA+1]=1; Av[4*LDA+2]=2; Av[4*LDA+3]=2; Av[4*LDA+4]=3; Av[4*LDA+5]=3;

	var JPVT = new Int32Array( 3 );
	var TAU = new Complex128Array( 3 );
	var WORK = new Complex128Array( 200 );
	var RWORK = new Float64Array( 2 * LDA );

	var info = zgeqp3( 3, 3, A, 1, LDA, 0, JPVT, 1, 0, TAU, 1, 0, WORK, 1, 0, 200, RWORK, 1, 0 );

	assertClose( info, 0, 1e-14, 'info' );
	// First column must be col 3 (largest norm), columns 2-3 can be either order
	assert.equal( JPVT[ 0 ], 3, 'first pivot is column 3' );
	// R(0,0) should be -sqrt(28) ~ -5.2915
	assertClose( Av[ 0 ], -5.2915026221291805, 1e-10, 'R(0,0)' );
	// R(1,1) and R(2,2) should be ~0 (rank deficient)
	assertClose( Math.abs( Av[ 2 * LDA + 2 ] ), 0.0, 1e-10, '|R(1,1)|~0' );
	assertClose( Math.abs( Av[ 4 * LDA + 4 ] ), 0.0, 1e-10, '|R(2,2)|~0' );
});

test( 'zgeqp3: N=0 quick return', function t() {
	var tc = findCase( 'n_zero' );
	var A = new Complex128Array( 10 );
	var JPVT = new Int32Array( 1 );
	var TAU = new Complex128Array( 1 );
	var WORK = new Complex128Array( 10 );
	var RWORK = new Float64Array( 10 );

	var info = zgeqp3( 3, 0, A, 1, 3, 0, JPVT, 1, 0, TAU, 1, 0, WORK, 1, 0, 10, RWORK, 1, 0 );
	assertClose( info, tc.info, 1e-14, 'info' );
});

test( 'zgeqp3: M=0 quick return', function t() {
	var tc = findCase( 'm_zero' );
	var A = new Complex128Array( 10 );
	var JPVT = new Int32Array( 3 );
	var TAU = new Complex128Array( 1 );
	var WORK = new Complex128Array( 10 );
	var RWORK = new Float64Array( 10 );

	var info = zgeqp3( 0, 3, A, 1, 1, 0, JPVT, 1, 0, TAU, 1, 0, WORK, 1, 0, 10, RWORK, 1, 0 );
	assertClose( info, tc.info, 1e-14, 'info' );
});

test( 'zgeqp3: 1x1 matrix', function t() {
	var tc = findCase( 'one_by_one' );
	var A = new Complex128Array( LDA * LDA );
	var Av = reinterpret( A, 0 );
	Av[0] = 3; Av[1] = 4;

	var JPVT = new Int32Array( [ 0 ] );
	var TAU = new Complex128Array( 1 );
	var WORK = new Complex128Array( 200 );
	var RWORK = new Float64Array( 2 * LDA );

	var info = zgeqp3( 1, 1, A, 1, LDA, 0, JPVT, 1, 0, TAU, 1, 0, WORK, 1, 0, 200, RWORK, 1, 0 );

	assertClose( info, tc.info, 1e-14, 'info' );
	assertArrayClose( Array.from( Av.subarray( 0, tc.a.length ) ), tc.a, 1e-10, 'a' );
	assertArrayClose( Array.from( reinterpret( TAU, 0 ) ), tc.tau, 1e-10, 'tau' );
	assert.deepStrictEqual( Array.from( JPVT ), tc.jpvt, 'jpvt' );
});

test( 'zgeqp3: fixed column', function t() {
	var tc = findCase( 'fixed_col' );
	var A = new Complex128Array( LDA * LDA );
	var Av = reinterpret( A, 0 );
	Av[0]=1; Av[1]=0; Av[2]=0; Av[3]=0; Av[4]=0; Av[5]=0;
	Av[2*LDA]=0; Av[2*LDA+1]=0; Av[2*LDA+2]=3; Av[2*LDA+3]=0; Av[2*LDA+4]=4; Av[2*LDA+5]=0;
	Av[4*LDA]=0; Av[4*LDA+1]=0; Av[4*LDA+2]=1; Av[4*LDA+3]=1; Av[4*LDA+4]=2; Av[4*LDA+5]=0;

	var JPVT = new Int32Array( [ 1, 0, 0 ] ); // Fix column 1
	var TAU = new Complex128Array( 3 );
	var WORK = new Complex128Array( 200 );
	var RWORK = new Float64Array( 2 * LDA );

	var info = zgeqp3( 3, 3, A, 1, LDA, 0, JPVT, 1, 0, TAU, 1, 0, WORK, 1, 0, 200, RWORK, 1, 0 );

	assertClose( info, tc.info, 1e-14, 'info' );
	assertArrayClose( Array.from( Av.subarray( 0, tc.a.length ) ), tc.a, 1e-10, 'a' );
	assertArrayClose( Array.from( reinterpret( TAU, 0 ) ), tc.tau, 1e-10, 'tau' );
	assert.deepStrictEqual( Array.from( JPVT ), tc.jpvt, 'jpvt' );
});

test( 'zgeqp3: fixed column with swap (JPVT[2]=1)', function t() {
	var A = new Complex128Array( LDA * LDA );
	var Av = reinterpret( A, 0 );
	Av[0]=1; Av[1]=0; Av[2]=2; Av[3]=0; Av[4]=0; Av[5]=0; Av[6]=1; Av[7]=0;
	Av[2*LDA]=3; Av[2*LDA+1]=0; Av[2*LDA+2]=0; Av[2*LDA+3]=1; Av[2*LDA+4]=2; Av[2*LDA+5]=0; Av[2*LDA+6]=1; Av[2*LDA+7]=1;
	Av[4*LDA]=0; Av[4*LDA+1]=1; Av[4*LDA+2]=1; Av[4*LDA+3]=0; Av[4*LDA+4]=3; Av[4*LDA+5]=0; Av[4*LDA+6]=2; Av[4*LDA+7]=0;

	var JPVT = new Int32Array( [ 0, 0, 1 ] ); // Fix column 3 (index 2) -- requires swap
	var TAU = new Complex128Array( 3 );
	var WORK = new Complex128Array( 200 );
	var RWORK = new Float64Array( 2 * LDA );

	var info = zgeqp3( 4, 3, A, 1, LDA, 0, JPVT, 1, 0, TAU, 1, 0, WORK, 1, 0, 200, RWORK, 1, 0 );

	assert.equal( info, 0, 'info' );
	// Column 3 was fixed, so it must appear first in the permutation
	assert.equal( JPVT[0], 3, 'fixed column 3 is first' );
	// R(0,0) should be non-zero (the norm of the fixed column)
	var r00mag = Math.sqrt( Av[0] * Av[0] + Av[1] * Av[1] );
	assert.ok( r00mag > 0.1, 'R(0,0) magnitude is non-trivial: ' + r00mag );
});

test( 'zgeqp3: two fixed columns', function t() {
	var A = new Complex128Array( LDA * LDA );
	var Av = reinterpret( A, 0 );
	Av[0]=1; Av[1]=0; Av[2]=2; Av[3]=0; Av[4]=0; Av[5]=0; Av[6]=1; Av[7]=0;
	Av[2*LDA]=3; Av[2*LDA+1]=0; Av[2*LDA+2]=0; Av[2*LDA+3]=1; Av[2*LDA+4]=2; Av[2*LDA+5]=0; Av[2*LDA+6]=1; Av[2*LDA+7]=1;
	Av[4*LDA]=0; Av[4*LDA+1]=1; Av[4*LDA+2]=1; Av[4*LDA+3]=0; Av[4*LDA+4]=3; Av[4*LDA+5]=0; Av[4*LDA+6]=2; Av[4*LDA+7]=0;

	var JPVT = new Int32Array( [ 1, 0, 1 ] ); // Fix columns 1 and 3
	var TAU = new Complex128Array( 3 );
	var WORK = new Complex128Array( 200 );
	var RWORK = new Float64Array( 2 * LDA );

	var info = zgeqp3( 4, 3, A, 1, LDA, 0, JPVT, 1, 0, TAU, 1, 0, WORK, 1, 0, 200, RWORK, 1, 0 );

	assert.equal( info, 0, 'info' );
	// Fixed columns 1 and 3 should be in positions 1 and 2 (JPVT[0] and JPVT[1])
	assert.ok( JPVT[0] === 1 || JPVT[0] === 3, 'first pivot is a fixed column: ' + JPVT[0] );
	assert.ok( JPVT[1] === 1 || JPVT[1] === 3, 'second pivot is a fixed column: ' + JPVT[1] );
	assert.notEqual( JPVT[0], JPVT[1], 'first two pivots are different' );
});

test( 'zgeqp3: large matrix 35x36 (blocked zlaqps path, sminmn > 32)', function t() {
	// Need min(M,N) > 32 to trigger blocked code
	var BLDA = 40;
	var M = 35;
	var N = 36;
	var A = new Complex128Array( BLDA * N );
	var Av = reinterpret( A, 0 );
	var i;
	var j;

	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			Av[ 2 * ( i + j * BLDA ) ] = ( ( ( i + 1 ) * ( j + 1 ) + 3 ) % 7 ) - 3.0;
			Av[ 2 * ( i + j * BLDA ) + 1 ] = ( ( ( i + 1 ) + ( j + 1 ) ) % 5 ) - 2.0;
		}
	}

	var JPVT = new Int32Array( N );
	var TAU = new Complex128Array( Math.min( M, N ) );
	var WORK = new Complex128Array( 10000 );
	var RWORK = new Float64Array( 2 * N );

	var info = zgeqp3( M, N, A, 1, BLDA, 0, JPVT, 1, 0, TAU, 1, 0, WORK, 1, 0, 10000, RWORK, 1, 0 );

	assert.equal( info, 0, 'info' );

	// Verify R diagonal has decreasing magnitude
	var prevMag = Infinity;
	var re;
	var im;
	var mag;
	var minmn = Math.min( M, N );
	for ( i = 0; i < minmn; i++ ) {
		re = Av[ 2 * ( i + i * BLDA ) ];
		im = Av[ 2 * ( i + i * BLDA ) + 1 ];
		mag = Math.sqrt( re * re + im * im );
		assert.ok( mag <= prevMag + 1e-10, 'R diagonal magnitude decreasing at ' + i + ': ' + mag + ' <= ' + prevMag );
		prevMag = mag;
	}

	// Verify JPVT is a valid permutation of 1..N
	var perm = Array.from( JPVT ).sort( function cmp( a, b ) { return a - b; } );
	for ( i = 0; i < N; i++ ) {
		assert.equal( perm[ i ], i + 1, 'JPVT is valid permutation at ' + i );
	}
});

test( 'zgeqp3: wide matrix 8x36 (blocked zlaqps path)', function t() {
	var BLDA = 40;
	var M = 8;
	var N = 36;
	var A = new Complex128Array( BLDA * N );
	var Av = reinterpret( A, 0 );
	var i;
	var j;

	// Fill A with same pattern as Fortran: dcmplx(mod(i*j+3,7) - 3, mod(i+j,5) - 2)
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			Av[ 2 * ( i + j * BLDA ) ] = ( ( ( i + 1 ) * ( j + 1 ) + 3 ) % 7 ) - 3.0;
			Av[ 2 * ( i + j * BLDA ) + 1 ] = ( ( ( i + 1 ) + ( j + 1 ) ) % 5 ) - 2.0;
		}
	}

	var JPVT = new Int32Array( N );
	var TAU = new Complex128Array( M );
	var WORK = new Complex128Array( 10000 );
	var RWORK = new Float64Array( 2 * BLDA );

	var info = zgeqp3( M, N, A, 1, BLDA, 0, JPVT, 1, 0, TAU, 1, 0, WORK, 1, 0, 10000, RWORK, 1, 0 );

	assert.equal( info, 0, 'info' );

	// Verify R diagonal has decreasing magnitude (property of QR with pivoting)
	var prevMag = Infinity;
	var re;
	var im;
	var mag;
	for ( i = 0; i < M; i++ ) {
		re = Av[ 2 * ( i + i * BLDA ) ];
		im = Av[ 2 * ( i + i * BLDA ) + 1 ];
		mag = Math.sqrt( re * re + im * im );
		assert.ok( mag <= prevMag + 1e-10, 'R diagonal magnitude decreasing at ' + i + ': ' + mag + ' <= ' + prevMag );
		prevMag = mag;
	}

	// Verify JPVT is a valid permutation of 1..N
	var perm = Array.from( JPVT ).sort( function cmp( a, b ) { return a - b; } );
	for ( i = 0; i < N; i++ ) {
		assert.equal( perm[ i ], i + 1, 'JPVT is valid permutation at ' + i );
	}
});
