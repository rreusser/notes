'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dlarfx = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlarfx.jsonl' ), 'utf8' ).trim().split( '\n' );
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
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

/**
* Build an MxN column-major matrix from flat column-major data.
* The JS routine uses strideC1=1, strideC2=M for column-major.
*/
function buildMatrix( M, N, values ) {
	var C = new Float64Array( M * N );
	var i;
	for ( i = 0; i < values.length && i < M * N; i++ ) {
		C[ i ] = values[ i ];
	}
	return C;
}

/**
* Extract MxN column-major flat array from C.
*/
function extractMatrix( C, M, N ) {
	var out = [];
	var i;
	for ( i = 0; i < M * N; i++ ) {
		out.push( C[ i ] );
	}
	return out;
}


// TESTS //

test( 'dlarfx: left M=2 N=3', function t() {
	var tc = findCase( 'left M=2 N=3' );
	var M = 2;
	var N = 3;
	var v = new Float64Array([ 1.0, 0.5 ]);
	var tau = 1.6;
	// Column-major: col0=[1,4], col1=[2,5], col2=[3,6]
	var C = new Float64Array([ 1, 4, 2, 5, 3, 6 ]);
	var WORK = new Float64Array( N );

	dlarfx( 'L', M, N, v, 1, 0, tau, C, 1, M, 0, WORK, 1, 0 );

	assertArrayClose( extractMatrix( C, M, N ), tc.C, 1e-12, 'C' );
});

test( 'dlarfx: right M=3 N=2', function t() {
	var tc = findCase( 'right M=3 N=2' );
	var M = 3;
	var N = 2;
	var v = new Float64Array([ 1.0, 0.5 ]);
	var tau = 1.6;
	// Column-major: col0=[1,3,5], col1=[2,4,6]
	var C = new Float64Array([ 1, 3, 5, 2, 4, 6 ]);
	var WORK = new Float64Array( M );

	dlarfx( 'R', M, N, v, 1, 0, tau, C, 1, M, 0, WORK, 1, 0 );

	assertArrayClose( extractMatrix( C, M, N ), tc.C, 1e-12, 'C' );
});

test( 'dlarfx: tau=0 does nothing', function t() {
	var tc = findCase( 'tau=0' );
	var M = 2;
	var N = 2;
	var v = new Float64Array([ 1.0, 0.5 ]);
	// Column-major: col0=[1,3], col1=[2,4]
	var C = new Float64Array([ 1, 3, 2, 4 ]);
	var WORK = new Float64Array( N );

	dlarfx( 'L', M, N, v, 1, 0, 0.0, C, 1, M, 0, WORK, 1, 0 );

	assertArrayClose( extractMatrix( C, M, N ), tc.C, 1e-14, 'C' );
});

test( 'dlarfx: left M=3 N=2', function t() {
	var tc = findCase( 'left M=3 N=2' );
	var M = 3;
	var N = 2;
	var v = new Float64Array([ 1.0, 0.3, -0.5 ]);
	var tau = 1.2;
	var C = new Float64Array([ 2, 3, 5, 1, 4, 6 ]);
	var WORK = new Float64Array( N );

	dlarfx( 'L', M, N, v, 1, 0, tau, C, 1, M, 0, WORK, 1, 0 );

	assertArrayClose( extractMatrix( C, M, N ), tc.C, 1e-12, 'C' );
});

test( 'dlarfx: right M=2 N=3', function t() {
	var tc = findCase( 'right M=2 N=3' );
	var M = 2;
	var N = 3;
	var v = new Float64Array([ 1.0, 0.3, -0.5 ]);
	var tau = 1.2;
	var C = new Float64Array([ 2, 4, 1, 5, 3, 6 ]);
	var WORK = new Float64Array( M );

	dlarfx( 'R', M, N, v, 1, 0, tau, C, 1, M, 0, WORK, 1, 0 );

	assertArrayClose( extractMatrix( C, M, N ), tc.C, 1e-12, 'C' );
});

test( 'dlarfx: left M=5 N=3', function t() {
	var tc = findCase( 'left M=5 N=3' );
	var M = 5;
	var N = 3;
	var v = new Float64Array([ 1.0, 0.2, -0.3, 0.4, -0.1 ]);
	var tau = 1.5;
	// Column-major: Fortran C(I,J) = I + (J-1)*5, stored col by col
	var C = new Float64Array( M * N );
	var i;
	var j;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			C[ i + j*M ] = ( i + 1 ) + j * 5;
		}
	}
	var WORK = new Float64Array( N );

	dlarfx( 'L', M, N, v, 1, 0, tau, C, 1, M, 0, WORK, 1, 0 );

	assertArrayClose( extractMatrix( C, M, N ), tc.C, 1e-12, 'C' );
});

test( 'dlarfx: left M=10 N=4', function t() {
	var tc = findCase( 'left M=10 N=4' );
	var M = 10;
	var N = 4;
	var v = new Float64Array( M );
	var i;
	var j;
	for ( i = 0; i < M; i++ ) {
		v[ i ] = 0.1 * ( i + 1 );
	}
	v[ 0 ] = 1.0;
	var tau = 1.8;
	var C = new Float64Array( M * N );
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			C[ i + j*M ] = ( i + 1 ) + 0.5 * ( j + 1 );
		}
	}
	var WORK = new Float64Array( N );

	dlarfx( 'L', M, N, v, 1, 0, tau, C, 1, M, 0, WORK, 1, 0 );

	assertArrayClose( extractMatrix( C, M, N ), tc.C, 1e-12, 'C' );
});

test( 'dlarfx: right M=4 N=10', function t() {
	var tc = findCase( 'right M=4 N=10' );
	var M = 4;
	var N = 10;
	var v = new Float64Array( N );
	var i;
	var j;
	for ( i = 0; i < N; i++ ) {
		v[ i ] = 0.1 * ( i + 1 );
	}
	v[ 0 ] = 1.0;
	var tau = 1.8;
	var C = new Float64Array( M * N );
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			C[ i + j*M ] = ( i + 1 ) + 0.5 * ( j + 1 );
		}
	}
	var WORK = new Float64Array( M );

	dlarfx( 'R', M, N, v, 1, 0, tau, C, 1, M, 0, WORK, 1, 0 );

	assertArrayClose( extractMatrix( C, M, N ), tc.C, 1e-12, 'C' );
});

test( 'dlarfx: left M=4 N=2', function t() {
	var tc = findCase( 'left M=4 N=2' );
	var M = 4;
	var N = 2;
	var v = new Float64Array([ 1.0, 0.3, -0.4, 0.2 ]);
	var tau = 1.4;
	var C = new Float64Array( M * N );
	var i;
	var j;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			C[ i + j*M ] = ( i + 1 ) + j * 4;
		}
	}
	var WORK = new Float64Array( N );

	dlarfx( 'L', M, N, v, 1, 0, tau, C, 1, M, 0, WORK, 1, 0 );

	assertArrayClose( extractMatrix( C, M, N ), tc.C, 1e-12, 'C' );
});

test( 'dlarfx: left M=6 N=3', function t() {
	var tc = findCase( 'left M=6 N=3' );
	var M = 6;
	var N = 3;
	var v = new Float64Array([ 1.0, 0.2, -0.3, 0.4, -0.1, 0.5 ]);
	var tau = 1.3;
	var C = new Float64Array( M * N );
	var i;
	var j;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			C[ i + j*M ] = ( i + 1 ) + j * 6;
		}
	}
	var WORK = new Float64Array( N );

	dlarfx( 'L', M, N, v, 1, 0, tau, C, 1, M, 0, WORK, 1, 0 );

	assertArrayClose( extractMatrix( C, M, N ), tc.C, 1e-12, 'C' );
});

test( 'dlarfx: left M=7 N=2', function t() {
	var tc = findCase( 'left M=7 N=2' );
	var M = 7;
	var N = 2;
	var v = new Float64Array([ 1.0, 0.2, -0.3, 0.4, -0.1, 0.5, -0.2 ]);
	var tau = 1.1;
	var C = new Float64Array( M * N );
	var i;
	var j;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			C[ i + j*M ] = ( i + 1 ) + j * 7;
		}
	}
	var WORK = new Float64Array( N );

	dlarfx( 'L', M, N, v, 1, 0, tau, C, 1, M, 0, WORK, 1, 0 );

	assertArrayClose( extractMatrix( C, M, N ), tc.C, 1e-12, 'C' );
});

test( 'dlarfx: left M=8 N=2', function t() {
	var tc = findCase( 'left M=8 N=2' );
	var M = 8;
	var N = 2;
	var v = new Float64Array([ 1.0, 0.1, -0.2, 0.3, -0.4, 0.15, -0.25, 0.35 ]);
	var tau = 1.6;
	var C = new Float64Array( M * N );
	var i;
	var j;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			C[ i + j*M ] = ( i + 1 ) + j * 8;
		}
	}
	var WORK = new Float64Array( N );

	dlarfx( 'L', M, N, v, 1, 0, tau, C, 1, M, 0, WORK, 1, 0 );

	assertArrayClose( extractMatrix( C, M, N ), tc.C, 1e-12, 'C' );
});

test( 'dlarfx: left M=9 N=2', function t() {
	var tc = findCase( 'left M=9 N=2' );
	var M = 9;
	var N = 2;
	var v = new Float64Array([ 1.0, 0.1, -0.2, 0.3, -0.4, 0.15, -0.25, 0.35, -0.05 ]);
	var tau = 1.7;
	var C = new Float64Array( M * N );
	var i;
	var j;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			C[ i + j*M ] = ( i + 1 ) + j * 9;
		}
	}
	var WORK = new Float64Array( N );

	dlarfx( 'L', M, N, v, 1, 0, tau, C, 1, M, 0, WORK, 1, 0 );

	assertArrayClose( extractMatrix( C, M, N ), tc.C, 1e-12, 'C' );
});
