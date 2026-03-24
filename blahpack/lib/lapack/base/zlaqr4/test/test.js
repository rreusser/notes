'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var zlaqr4 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zlaqr4.jsonl' ), 'utf8' ).trim().split( '\n' );
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

function makeMatrix( N ) {
	return {
		data: new Complex128Array( N * N ),
		s1: 1,
		s2: N,
		offset: 0
	};
}

function mset( m, N, i, j, re, im ) {
	var mv = reinterpret( m.data, 0 );
	var idx = ( m.offset + i * m.s1 + j * m.s2 ) * 2;
	mv[ idx ] = re;
	mv[ idx + 1 ] = im;
}

function getFlat( m ) {
	return Array.from( reinterpret( m.data, 0 ) );
}

function buildHess6( Hm ) {
	var n = 6;
	mset( Hm, n, 0, 0, 6.0, 1.0 );
	mset( Hm, n, 0, 1, 1.0, -0.5 );
	mset( Hm, n, 0, 2, 0.5, 0.0 );
	mset( Hm, n, 0, 3, 0.25, 0.1 );
	mset( Hm, n, 0, 4, 0.1, 0.0 );
	mset( Hm, n, 0, 5, 0.05, -0.05 );
	mset( Hm, n, 1, 0, 1.0, 0.0 );
	mset( Hm, n, 1, 1, 5.0, -1.0 );
	mset( Hm, n, 1, 2, 1.0, 0.5 );
	mset( Hm, n, 1, 3, 0.5, 0.0 );
	mset( Hm, n, 1, 4, 0.25, -0.1 );
	mset( Hm, n, 1, 5, 0.1, 0.0 );
	mset( Hm, n, 2, 1, 0.8, 0.2 );
	mset( Hm, n, 2, 2, 4.0, 0.5 );
	mset( Hm, n, 2, 3, 1.0, -0.5 );
	mset( Hm, n, 2, 4, 0.5, 0.0 );
	mset( Hm, n, 2, 5, 0.25, 0.1 );
	mset( Hm, n, 3, 2, 0.6, -0.1 );
	mset( Hm, n, 3, 3, 3.0, -0.5 );
	mset( Hm, n, 3, 4, 1.0, 0.5 );
	mset( Hm, n, 3, 5, 0.5, 0.0 );
	mset( Hm, n, 4, 3, 0.4, 0.15 );
	mset( Hm, n, 4, 4, 2.0, 0.0 );
	mset( Hm, n, 4, 5, 1.0, -0.5 );
	mset( Hm, n, 5, 4, 0.2, -0.1 );
	mset( Hm, n, 5, 5, 1.0, 1.0 );
}


// TESTS //

test( 'zlaqr4: main export is a function', function t() {
	assert.strictEqual( typeof zlaqr4, 'function' );
});

test( 'zlaqr4: n_eq_0', function t() {
	var tc = findCase( 'n_eq_0' );
	var H = new Complex128Array( 0 );
	var Z = new Complex128Array( 0 );
	var W = new Complex128Array( 0 );
	var WORK = new Complex128Array( 1 );

	var info = zlaqr4( true, false, 0, 1, 0,
		H, 1, 0, 0,
		W, 1, 0,
		1, 0,
		Z, 1, 0, 0,
		WORK, 1, 0, 1
	);
	assert.equal( info, tc.info );
});

test( 'zlaqr4: n_eq_1', function t() {
	var tc = findCase( 'n_eq_1' );
	var n = 1;
	var Hm = makeMatrix( n );
	var Zm = makeMatrix( n );
	var W = new Complex128Array( n );
	var WORK = new Complex128Array( 1 );

	mset( Hm, n, 0, 0, 3.0, 2.0 );
	mset( Zm, n, 0, 0, 1.0, 0.0 );

	var info = zlaqr4( true, true, n, 1, 1,
		Hm.data, Hm.s1, Hm.s2, Hm.offset,
		W, 1, 0,
		1, 1,
		Zm.data, Zm.s1, Zm.s2, Zm.offset,
		WORK, 1, 0, 1
	);
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( reinterpret( W, 0 ) ), tc.w, 1e-13, 'w' );
});

test( 'zlaqr4: 6x6 Schur form with Z (small, uses zlahqr)', function t() {
	var tc = findCase( '6x6_schur_with_Z' );
	var n = 6;
	var Hm = makeMatrix( n );
	var Zm = makeMatrix( n );
	var W = new Complex128Array( n );
	var WORK = new Complex128Array( n * n );
	var i;

	buildHess6( Hm );

	for ( i = 0; i < n; i++ ) {
		mset( Zm, n, i, i, 1.0, 0.0 );
	}

	var info = zlaqr4( true, true, n, 1, 6,
		Hm.data, Hm.s1, Hm.s2, Hm.offset,
		W, 1, 0,
		1, 6,
		Zm.data, Zm.s1, Zm.s2, Zm.offset,
		WORK, 1, 0, n * n
	);
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( reinterpret( W, 0 ) ), tc.w, 1e-10, 'w' );
	assertArrayClose( getFlat( Hm ), tc.H, 1e-10, 'H' );
	assertArrayClose( getFlat( Zm ), tc.Z, 1e-10, 'Z' );
});

test( 'zlaqr4: 6x6 eigenvalues only', function t() {
	var tc = findCase( '6x6_eig_only' );
	var n = 6;
	var Hm = makeMatrix( n );
	var Zm = makeMatrix( n );
	var W = new Complex128Array( n );
	var WORK = new Complex128Array( n * n );

	buildHess6( Hm );

	var info = zlaqr4( false, false, n, 1, 6,
		Hm.data, Hm.s1, Hm.s2, Hm.offset,
		W, 1, 0,
		1, 6,
		Zm.data, Zm.s1, Zm.s2, Zm.offset,
		WORK, 1, 0, n * n
	);
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( reinterpret( W, 0 ) ), tc.w, 1e-10, 'w' );
});

test( 'zlaqr4: 15x15 multishift', function t() {
	var tc = findCase( '15x15_multishift' );
	var n = 15;
	var Hm = makeMatrix( n );
	var Zm = makeMatrix( n );
	var W = new Complex128Array( n );
	var WORK = new Complex128Array( n * n );
	var i;
	var j;

	// Build same diagonally dominant Hessenberg as Fortran test
	for ( i = 0; i < n; i++ ) {
		mset( Hm, n, i, i, (n - i) * 1.0, (i + 1) * 0.1 );
		for ( j = i + 1; j < Math.min( i + 4, n ); j++ ) {
			mset( Hm, n, i, j, 0.5 / (j - i), 0.1 * Math.pow( -1, j + 1 ) );
		}
		if ( i < n - 1 ) {
			mset( Hm, n, i + 1, i, 0.3 + 0.1 * (i + 1), 0.05 * Math.pow( -1, i + 1 ) );
		}
	}

	for ( i = 0; i < n; i++ ) {
		mset( Zm, n, i, i, 1.0, 0.0 );
	}

	var info = zlaqr4( true, true, n, 1, 15,
		Hm.data, Hm.s1, Hm.s2, Hm.offset,
		W, 1, 0,
		1, 15,
		Zm.data, Zm.s1, Zm.s2, Zm.offset,
		WORK, 1, 0, n * n
	);
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( reinterpret( W, 0 ) ), tc.w, 1e-8, 'w' );
	assertArrayClose( getFlat( Hm ), tc.H, 1e-8, 'H' );
});
