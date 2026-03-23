'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zhetrd = require( '../../zhetrd/lib/base.js' );
var zungtr = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zungtr.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	var relErr;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		relErr = Math.abs( actual[ i ] - expected[ i ] ) / Math.max( Math.abs( expected[ i ] ), 1.0 );
		assert.ok( relErr <= tol, msg + '[' + i + ']: expected ' + expected[ i ] + ', got ' + actual[ i ] );
	}
}

/**
* Verify Q^H * Q = I (unitarity).
*/
function verifyUnitary( Q, N, tol, msg ) {
	var dotRe;
	var dotIm;
	var expRe;
	var i;
	var j;
	var k;

	for ( i = 0; i < N; i++ ) {
		for ( j = 0; j < N; j++ ) {
			dotRe = 0.0;
			dotIm = 0.0;
			for ( k = 0; k < N; k++ ) {
				var qiRe = Q[ (i * N + k) * 2 ];
				var qiIm = Q[ (i * N + k) * 2 + 1 ];
				var qjRe = Q[ (j * N + k) * 2 ];
				var qjIm = Q[ (j * N + k) * 2 + 1 ];
				dotRe += qiRe * qjRe + qiIm * qjIm;
				dotIm += qiRe * qjIm - qiIm * qjRe;
			}
			expRe = ( i === j ) ? 1.0 : 0.0;
			assert.ok( Math.abs(dotRe - expRe) < tol && Math.abs(dotIm) < tol,
				msg + ': Q^H*Q[' + i + ',' + j + '] = (' + dotRe + ',' + dotIm + ')' );
		}
	}
}

/**
* Helper to run zhetrd + zungtr together.
*/
function runZungtr( uplo, N, Adata ) {
	var lwork = Math.max( 1, (32 + 1) * N );
	var WORK = new Complex128Array( lwork );
	var TAU = new Complex128Array( Math.max( 1, N - 1 ) );
	var d = new Float64Array( N );
	var e = new Float64Array( Math.max( 1, N - 1 ) );
	var A = new Complex128Array( Adata.buffer.slice(0) );
	var info;

	// First reduce to tridiagonal
	zhetrd(uplo, N, A, 1, N, 0, d, 1, 0, e, 1, 0, TAU, 1, 0 );

	// Then generate Q
	info = zungtr(uplo, N, A, 1, N, 0, TAU, 1, 0, WORK, 1, 0 );

	return {
		info: info,
		Q: new Float64Array( A.buffer )
	};
}


// TESTS //

test( 'zungtr: 4x4 UPLO=L', function t() {
	var tc = findCase( 'zungtr_4x4_L' );
	var Adata = new Float64Array([
		2.0, 0.0, 1.0, 1.0, 0.5, -0.5, 0.0, 0.0,
		0.0, 0.0, 3.0, 0.0, 0.0, 2.0, 1.0, -1.0,
		0.0, 0.0, 0.0, 0.0, 4.0, 0.0, 0.5, 0.5,
		0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 5.0, 0.0
	]);
	var result = runZungtr( 'lower', 4, Adata );

	assert.equal( result.info, 0, 'info=0' );
	verifyUnitary( result.Q, 4, 1e-12, 'Q unitarity' );
	assertArrayClose( Array.from( result.Q ), tc.Q, 1e-13, 'Q values' );
});

test( 'zungtr: 4x4 UPLO=U', function t() {
	var tc = findCase( 'zungtr_4x4_U' );
	var Adata = new Float64Array([
		2.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
		1.0, -1.0, 3.0, 0.0, 0.0, 0.0, 0.0, 0.0,
		0.5, 0.5, 0.0, -2.0, 4.0, 0.0, 0.0, 0.0,
		0.0, 0.0, 1.0, 1.0, 0.5, -0.5, 5.0, 0.0
	]);
	var result = runZungtr( 'upper', 4, Adata );

	assert.equal( result.info, 0, 'info=0' );
	verifyUnitary( result.Q, 4, 1e-12, 'Q unitarity' );
	assertArrayClose( Array.from( result.Q ), tc.Q, 1e-13, 'Q values' );
});

test( 'zungtr: 3x3 UPLO=L', function t() {
	var tc = findCase( 'zungtr_3x3_L' );
	// LDA=4 in the Fortran test, so the fixture has LDA=4 stride
	// Our JS test uses N=3, LDA=3
	var Adata = new Float64Array([
		4.0, 0.0, 1.0, -2.0, 0.0, 1.0,
		0.0, 0.0, 5.0, 0.0, 2.0, 0.0,
		0.0, 0.0, 0.0, 0.0, 6.0, 0.0
	]);
	var result = runZungtr( 'lower', 3, Adata );

	assert.equal( result.info, 0, 'info=0' );
	verifyUnitary( result.Q, 3, 1e-12, 'Q unitarity' );

	// Compare with fixture, but fixture used LDA=4, we need to extract 3x3 portion
	// Fixture Q is interleaved, LDA=4, so each column is 4 complex entries (8 doubles)
	// We only compare the 3x3 portion
	var expected3x3 = [];
	var j;
	var i;
	for ( j = 0; j < 3; j++ ) {
		for ( i = 0; i < 3; i++ ) {
			expected3x3.push( tc.Q[ j * 8 + i * 2 ] );     // real
			expected3x3.push( tc.Q[ j * 8 + i * 2 + 1 ] ); // imag
		}
	}
	assertArrayClose( Array.from( result.Q ), expected3x3, 1e-13, 'Q values' );
});

test( 'zungtr: N=1', function t() {
	var tc = findCase( 'zungtr_1x1' );
	var Adata = new Float64Array([ 5.0, 0.0 ]);
	var A = new Complex128Array( Adata.buffer.slice(0) );
	var TAU = new Complex128Array( 1 );
	var WORK = new Complex128Array( 1 );
	var d = new Float64Array( 1 );
	var e = new Float64Array( 1 );

	zhetrd('lower', 1, A, 1, 1, 0, d, 1, 0, e, 1, 0, TAU, 1, 0 );
	var info = zungtr('lower', 1, A, 1, 1, 0, TAU, 1, 0, WORK, 1, 0 );

	assert.equal( info, 0, 'info=0' );
	// Q should be identity for N=1
	var Qv = reinterpret( A, 0 );
	assert.ok( Math.abs( Qv[0] - 1.0 ) < 1e-15, 'Q(0,0) = 1' );
	assert.ok( Math.abs( Qv[1] ) < 1e-15, 'Q(0,0) imag = 0' );
});

test( 'zungtr: N=0', function t() {
	var A = new Complex128Array( 0 );
	var TAU = new Complex128Array( 0 );
	var WORK = new Complex128Array( 1 );

	var info = zungtr('lower', 0, A, 1, 1, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.equal( info, 0, 'info=0' );
});
