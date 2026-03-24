'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dlaqr4 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlaqr4.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual + ' (relErr=' + relErr + ')' );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch (' + actual.length + ' vs ' + expected.length + ')' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

/**
* Build an NxN column-major matrix (strideH1=1, strideH2=N).
*/
function buildHessenberg6x6() {
	var N = 6;
	var H = new Float64Array( N * N );
	H[ 0 + 0*N ] = 4.0; H[ 0 + 1*N ] = 1.0; H[ 0 + 2*N ] = 0.5; H[ 0 + 3*N ] = 0.2; H[ 0 + 4*N ] = 0.1; H[ 0 + 5*N ] = 0.05;
	H[ 1 + 0*N ] = 1.0; H[ 1 + 1*N ] = 3.0; H[ 1 + 2*N ] = 1.0; H[ 1 + 3*N ] = 0.3; H[ 1 + 4*N ] = 0.2; H[ 1 + 5*N ] = 0.1;
	H[ 2 + 1*N ] = 0.8; H[ 2 + 2*N ] = 2.0; H[ 2 + 3*N ] = 1.0; H[ 2 + 4*N ] = 0.4; H[ 2 + 5*N ] = 0.2;
	H[ 3 + 2*N ] = 0.6; H[ 3 + 3*N ] = 1.0; H[ 3 + 4*N ] = 0.5; H[ 3 + 5*N ] = 0.3;
	H[ 4 + 3*N ] = 0.4; H[ 4 + 4*N ] = -1.0; H[ 4 + 5*N ] = 1.0;
	H[ 5 + 4*N ] = 0.3; H[ 5 + 5*N ] = -2.0;
	return H;
}

function identityMatrix( N ) {
	var Z = new Float64Array( N * N );
	var i;
	for ( i = 0; i < N; i++ ) {
		Z[ i + i*N ] = 1.0;
	}
	return Z;
}


// TESTS //

test( 'dlaqr4: 6x6 wantt=true wantz=true', function t() {
	var tc = findCase( '6x6 wantt wantz' );
	var N = 6;
	var H = buildHessenberg6x6();
	var Z = identityMatrix( N );
	var WR = new Float64Array( N );
	var WI = new Float64Array( N );
	var WORK = new Float64Array( 10000 );

	var info = dlaqr4( true, true, N, 1, N, H, 1, N, 0, WR, 1, 0, WI, 1, 0, 1, N, Z, 1, N, 0, WORK, 1, 0, 10000 );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( Array.from( WR ), tc.WR, 1e-12, 'WR' );
	assertArrayClose( Array.from( WI ), tc.WI, 1e-12, 'WI' );
	assertArrayClose( Array.from( H ), tc.H, 1e-12, 'H' );
	assertArrayClose( Array.from( Z ), tc.Z, 1e-12, 'Z' );
});

test( 'dlaqr4: 6x6 wantt=true wantz=false', function t() {
	var tc = findCase( '6x6 wantt no wantz' );
	var N = 6;
	var H = buildHessenberg6x6();
	var Z = new Float64Array( N * N );
	var WR = new Float64Array( N );
	var WI = new Float64Array( N );
	var WORK = new Float64Array( 10000 );

	var info = dlaqr4( true, false, N, 1, N, H, 1, N, 0, WR, 1, 0, WI, 1, 0, 1, N, Z, 1, N, 0, WORK, 1, 0, 10000 );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( Array.from( WR ), tc.WR, 1e-12, 'WR' );
	assertArrayClose( Array.from( WI ), tc.WI, 1e-12, 'WI' );
	assertArrayClose( Array.from( H ), tc.H, 1e-12, 'H' );
});

test( 'dlaqr4: 6x6 wantt=false wantz=false (eigenvalues only)', function t() {
	var tc = findCase( '6x6 no wantt no wantz' );
	var N = 6;
	var H = buildHessenberg6x6();
	var Z = new Float64Array( N * N );
	var WR = new Float64Array( N );
	var WI = new Float64Array( N );
	var WORK = new Float64Array( 10000 );

	var info = dlaqr4( false, false, N, 1, N, H, 1, N, 0, WR, 1, 0, WI, 1, 0, 1, N, Z, 1, N, 0, WORK, 1, 0, 10000 );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( Array.from( WR ), tc.WR, 1e-12, 'WR' );
	assertArrayClose( Array.from( WI ), tc.WI, 1e-12, 'WI' );
});

test( 'dlaqr4: 15x15 wantt=true wantz=true (multishift path)', function t() {
	var tc = findCase( '15x15 wantt wantz' );
	var N = 15;
	var H = new Float64Array( N * N );
	var i;
	var j;
	for ( i = 0; i < N; i++ ) {
		H[ i + i*N ] = ( N - i ) * 1.0;
		if ( i < N - 1 ) {
			H[ ( i + 1 ) + i*N ] = 1.0;
		}
		for ( j = i + 1; j < N; j++ ) {
			H[ i + j*N ] = 0.5 / ( j - i );
		}
	}
	var Z = identityMatrix( N );
	var WR = new Float64Array( N );
	var WI = new Float64Array( N );
	var WORK = new Float64Array( 10000 );

	var info = dlaqr4( true, true, N, 1, N, H, 1, N, 0, WR, 1, 0, WI, 1, 0, 1, N, Z, 1, N, 0, WORK, 1, 0, 10000 );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( Array.from( WR ), tc.WR, 1e-10, 'WR' );
	assertArrayClose( Array.from( WI ), tc.WI, 1e-10, 'WI' );
	assertArrayClose( Array.from( H ), tc.H, 1e-10, 'H' );
	assertArrayClose( Array.from( Z ), tc.Z, 1e-10, 'Z' );
});

test( 'dlaqr4: N=0 quick return', function t() {
	var tc = findCase( 'N=0' );
	var H = new Float64Array( 1 );
	var Z = new Float64Array( 1 );
	var WR = new Float64Array( 1 );
	var WI = new Float64Array( 1 );
	var WORK = new Float64Array( 1 );

	var info = dlaqr4( true, true, 0, 1, 0, H, 1, 1, 0, WR, 1, 0, WI, 1, 0, 1, 0, Z, 1, 1, 0, WORK, 1, 0, 1 );

	assert.strictEqual( info, tc.info, 'info' );
});

test( 'dlaqr4: N=1 trivial', function t() {
	var tc = findCase( 'N=1' );
	var N = 1;
	var H = new Float64Array([ 7.0 ]);
	var Z = new Float64Array([ 1.0 ]);
	var WR = new Float64Array( 1 );
	var WI = new Float64Array( 1 );
	var WORK = new Float64Array( 10 );

	var info = dlaqr4( true, true, N, 1, N, H, 1, N, 0, WR, 1, 0, WI, 1, 0, 1, N, Z, 1, N, 0, WORK, 1, 0, 10 );

	assert.strictEqual( info, tc.info, 'info' );
	assertClose( WR[ 0 ], tc.WR1, 1e-14, 'WR[0]' );
	assertClose( WI[ 0 ], tc.WI1, 1e-14, 'WI[0]' );
});

test( 'dlaqr4: 6x6 partial range ilo=2 ihi=5', function t() {
	var tc = findCase( '6x6 partial ilo=2 ihi=5' );
	var N = 6;
	var H = new Float64Array( N * N );
	H[ 0 + 0*N ] = 10.0;
	H[ 0 + 1*N ] = 0.5; H[ 0 + 2*N ] = 0.3; H[ 0 + 3*N ] = 0.2; H[ 0 + 4*N ] = 0.1; H[ 0 + 5*N ] = 0.05;
	H[ 1 + 1*N ] = 4.0; H[ 1 + 2*N ] = 1.0; H[ 1 + 3*N ] = 0.3; H[ 1 + 4*N ] = 0.2; H[ 1 + 5*N ] = 0.1;
	H[ 2 + 1*N ] = 0.8; H[ 2 + 2*N ] = 3.0; H[ 2 + 3*N ] = 0.5; H[ 2 + 4*N ] = 0.3; H[ 2 + 5*N ] = 0.15;
	H[ 3 + 2*N ] = 0.6; H[ 3 + 3*N ] = 2.0; H[ 3 + 4*N ] = 0.4; H[ 3 + 5*N ] = 0.2;
	H[ 4 + 3*N ] = 0.4; H[ 4 + 4*N ] = 1.0; H[ 4 + 5*N ] = 0.3;
	H[ 5 + 5*N ] = -5.0;
	var Z = identityMatrix( N );
	var WR = new Float64Array( N );
	var WI = new Float64Array( N );
	var WORK = new Float64Array( 10000 );

	var info = dlaqr4( true, true, N, 2, 5, H, 1, N, 0, WR, 1, 0, WI, 1, 0, 1, N, Z, 1, N, 0, WORK, 1, 0, 10000 );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( Array.from( WR ), tc.WR, 1e-12, 'WR' );
	assertArrayClose( Array.from( WI ), tc.WI, 1e-12, 'WI' );
	assertArrayClose( Array.from( H ), tc.H, 1e-12, 'H' );
	assertArrayClose( Array.from( Z ), tc.Z, 1e-12, 'Z' );
});
