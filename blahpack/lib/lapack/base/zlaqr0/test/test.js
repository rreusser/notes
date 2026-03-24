'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var zlaqr0 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zlaqr0.jsonl' ), 'utf8' ).trim().split( '\n' );
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

function assertUpperTriangular( Hm, n, tol, msg ) {
	var Hv = reinterpret( Hm.data, 0 );
	var i;
	var j;
	var idx;
	var re;
	var im;
	for ( j = 0; j < n; j++ ) {
		for ( i = j + 2; i < n; i++ ) {
			idx = ( Hm.offset + i * Hm.s1 + j * Hm.s2 ) * 2;
			re = Hv[ idx ];
			im = Hv[ idx + 1 ];
			assert.ok( Math.abs( re ) + Math.abs( im ) <= tol,
				msg + ': H(' + ( i + 1 ) + ',' + ( j + 1 ) + ') = (' + re + ',' + im + ') should be zero' );
		}
	}
}

function assertEigenvaluesMatch( actual, expected, tol, msg ) {
	var act = [];
	var exp = [];
	var i;
	for ( i = 0; i < actual.length; i += 2 ) {
		act.push( [ actual[ i ], actual[ i + 1 ] ] );
		exp.push( [ expected[ i ], expected[ i + 1 ] ] );
	}
	function cmp( a, b ) {
		if ( Math.abs( a[ 0 ] - b[ 0 ] ) > 1e-8 ) { return a[ 0 ] - b[ 0 ]; }
		return a[ 1 ] - b[ 1 ];
	}
	act.sort( cmp );
	exp.sort( cmp );
	for ( i = 0; i < act.length; i++ ) {
		assertClose( act[ i ][ 0 ], exp[ i ][ 0 ], tol, msg + '[' + i + '].re' );
		assertClose( act[ i ][ 1 ], exp[ i ][ 1 ], tol, msg + '[' + i + '].im' );
	}
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

test( 'zlaqr0: main export is a function', function t() {
	assert.strictEqual( typeof zlaqr0, 'function' );
});

test( 'zlaqr0: n_eq_0', function t() {
	var tc = findCase( 'n_eq_0' );
	var H = new Complex128Array( 0 );
	var Z = new Complex128Array( 0 );
	var W = new Complex128Array( 0 );
	var WORK = new Complex128Array( 1 );

	var info = zlaqr0( true, false, 0, 1, 0,
		H, 1, 0, 0,
		W, 1, 0,
		1, 0,
		Z, 1, 0, 0,
		WORK, 1, 0, 1
	);
	assert.equal( info, tc.info );
});

test( 'zlaqr0: n_eq_1', function t() {
	var tc = findCase( 'n_eq_1' );
	var n = 1;
	var Hm = makeMatrix( n );
	var Zm = makeMatrix( n );
	var W = new Complex128Array( n );
	var WORK = new Complex128Array( 1 );

	mset( Hm, n, 0, 0, 5.0, -3.0 );
	mset( Zm, n, 0, 0, 1.0, 0.0 );

	var info = zlaqr0( true, true, n, 1, 1,
		Hm.data, Hm.s1, Hm.s2, Hm.offset,
		W, 1, 0,
		1, 1,
		Zm.data, Zm.s1, Zm.s2, Zm.offset,
		WORK, 1, 0, 1
	);
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( reinterpret( W, 0 ) ), tc.w, 1e-13, 'w' );
});

test( 'zlaqr0: 6x6 Schur form with Z', function t() {
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

	var info = zlaqr0( true, true, n, 1, 6,
		Hm.data, Hm.s1, Hm.s2, Hm.offset,
		W, 1, 0,
		1, 6,
		Zm.data, Zm.s1, Zm.s2, Zm.offset,
		WORK, 1, 0, n * n
	);
	assert.equal( info, tc.info );
	assertEigenvaluesMatch( Array.from( reinterpret( W, 0 ) ), tc.w, 1e-10, 'w' );
	assertUpperTriangular( Hm, n, 1e-10, 'H' );
});

test( 'zlaqr0: 6x6 eigenvalues only', function t() {
	var tc = findCase( '6x6_eig_only' );
	var n = 6;
	var Hm = makeMatrix( n );
	var Zm = makeMatrix( n );
	var W = new Complex128Array( n );
	var WORK = new Complex128Array( n * n );

	buildHess6( Hm );

	var info = zlaqr0( false, false, n, 1, 6,
		Hm.data, Hm.s1, Hm.s2, Hm.offset,
		W, 1, 0,
		1, 6,
		Zm.data, Zm.s1, Zm.s2, Zm.offset,
		WORK, 1, 0, n * n
	);
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( reinterpret( W, 0 ) ), tc.w, 1e-10, 'w' );
});

test( 'zlaqr0: 15x15 multishift', function t() {
	var tc = findCase( '15x15_multishift' );
	var n = 15;
	var Hm = makeMatrix( n );
	var Zm = makeMatrix( n );
	var W = new Complex128Array( n );
	var WORK = new Complex128Array( n * n );
	var i;
	var j;

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

	var info = zlaqr0( true, true, n, 1, 15,
		Hm.data, Hm.s1, Hm.s2, Hm.offset,
		W, 1, 0,
		1, 15,
		Zm.data, Zm.s1, Zm.s2, Zm.offset,
		WORK, 1, 0, n * n
	);
	assert.equal( info, tc.info );
	assertEigenvaluesMatch( Array.from( reinterpret( W, 0 ) ), tc.w, 1e-8, 'w' );
	assertUpperTriangular( Hm, n, 1e-10, 'H' );
});

test( 'zlaqr0: 16x16 multishift eigenvalues with Z (property-based)', function t() {
	var n = 16;
	var Hm = makeMatrix( n );
	var Zm = makeMatrix( n );
	var W = new Complex128Array( n );
	var WORK = new Complex128Array( n * n );
	var Wv;
	var info;
	var i;

	// Build simple upper Hessenberg with well-separated real diagonal
	for ( i = 0; i < n; i++ ) {
		mset( Hm, n, i, i, ( n - i ) * 10.0, 0.0 );
		if ( i < n - 1 ) {
			mset( Hm, n, i + 1, i, 1.0, 0.0 );
		}
	}

	for ( i = 0; i < n; i++ ) {
		mset( Zm, n, i, i, 1.0, 0.0 );
	}

	// Use wantt=false, wantz=true to exercise multishift deflation+Z update path
	info = zlaqr0( false, true, n, 1, n,
		Hm.data, Hm.s1, Hm.s2, Hm.offset,
		W, 1, 0,
		1, n,
		Zm.data, Zm.s1, Zm.s2, Zm.offset,
		WORK, 1, 0, n * n
	);

	assert.equal( info, 0, 'convergence (info=0)' );

	// Eigenvalues should have reasonable magnitudes and not be NaN
	Wv = reinterpret( W, 0 );
	for ( i = 0; i < n; i++ ) {
		assert.ok( !Number.isNaN( Wv[ 2 * i ] ) && !Number.isNaN( Wv[ 2 * i + 1 ] ),
			'eigenvalue ' + i + ' is not NaN' );
		assert.ok( Math.abs( Wv[ 2 * i ] ) + Math.abs( Wv[ 2 * i + 1 ] ) < 1e6,
			'eigenvalue ' + i + ' has reasonable magnitude' );
	}
});

test( 'zlaqr0: 16x16 eigenvalues only (property-based)', function t() {
	var n = 16;
	var Hm = makeMatrix( n );
	var Zm = makeMatrix( n );
	var W = new Complex128Array( n );
	var WORK = new Complex128Array( n * n );
	var Wv;
	var info;
	var i;

	// Build simple upper Hessenberg
	for ( i = 0; i < n; i++ ) {
		mset( Hm, n, i, i, ( n - i ) * 10.0, 0.0 );
		if ( i < n - 1 ) {
			mset( Hm, n, i + 1, i, 1.0, 0.0 );
		}
	}

	info = zlaqr0( false, false, n, 1, n,
		Hm.data, Hm.s1, Hm.s2, Hm.offset,
		W, 1, 0,
		1, n,
		Zm.data, Zm.s1, Zm.s2, Zm.offset,
		WORK, 1, 0, n * n
	);

	assert.equal( info, 0, 'convergence (info=0)' );

	// All eigenvalues should be finite
	Wv = reinterpret( W, 0 );
	for ( i = 0; i < n; i++ ) {
		assert.ok( Number.isFinite( Wv[ 2 * i ] ) && Number.isFinite( Wv[ 2 * i + 1 ] ),
			'eigenvalue ' + i + ' is finite' );
	}
});

test( 'zlaqr0: workspace query (lwork=-1)', function t() {
	var n = 20;
	var Hm = makeMatrix( n );
	var Zm = makeMatrix( n );
	var W = new Complex128Array( n );
	var WORK = new Complex128Array( 1 );

	var info = zlaqr0( true, true, n, 1, n,
		Hm.data, Hm.s1, Hm.s2, Hm.offset,
		W, 1, 0,
		1, n,
		Zm.data, Zm.s1, Zm.s2, Zm.offset,
		WORK, 1, 0, -1
	);
	assert.equal( info, 0, 'workspace query returns info=0' );
});
