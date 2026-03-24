'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var zlaqr5 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zlaqr5.jsonl' ), 'utf8' ).trim().split( '\n' );
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

function getFlat( m, N ) {
	return Array.from( reinterpret( m.data, 0 ) );
}


// TESTS //

test( 'zlaqr5: main export is a function', function t() {
	assert.strictEqual( typeof zlaqr5, 'function' );
});

test( 'zlaqr5: 6x6 with 2 shifts', function t() {
	var tc = findCase( '6x6_2shifts' );
	var n = 6;
	var nshfts = 2;
	var Hm = makeMatrix( n );
	var Zm = makeMatrix( n );
	var Vm = makeMatrix( 3 ); // V is 3 x nshfts/2
	var Um;
	var WVm;
	var WHm;
	var S = new Complex128Array( nshfts );
	var Sv;
	var nu;
	var i;

	// Build H
	mset( Hm, n, 0, 0, 5.0, 1.0 );
	mset( Hm, n, 0, 1, 2.0, -0.5 );
	mset( Hm, n, 0, 2, 1.0, 0.0 );
	mset( Hm, n, 0, 3, 0.5, 0.2 );
	mset( Hm, n, 0, 4, 0.25, 0.0 );
	mset( Hm, n, 0, 5, 0.1, -0.1 );
	mset( Hm, n, 1, 0, 1.0, 0.0 );
	mset( Hm, n, 1, 1, 4.0, -1.0 );
	mset( Hm, n, 1, 2, 1.5, 0.5 );
	mset( Hm, n, 1, 3, 0.5, 0.0 );
	mset( Hm, n, 1, 4, 0.3, 0.1 );
	mset( Hm, n, 1, 5, 0.15, 0.0 );
	mset( Hm, n, 2, 1, 0.8, 0.2 );
	mset( Hm, n, 2, 2, 3.0, 0.5 );
	mset( Hm, n, 2, 3, 1.0, -0.5 );
	mset( Hm, n, 2, 4, 0.5, 0.0 );
	mset( Hm, n, 2, 5, 0.25, 0.1 );
	mset( Hm, n, 3, 2, 0.6, -0.1 );
	mset( Hm, n, 3, 3, 2.5, -0.5 );
	mset( Hm, n, 3, 4, 1.0, 0.5 );
	mset( Hm, n, 3, 5, 0.5, 0.0 );
	mset( Hm, n, 4, 3, 0.4, 0.15 );
	mset( Hm, n, 4, 4, 2.0, 0.0 );
	mset( Hm, n, 4, 5, 1.0, -0.5 );
	mset( Hm, n, 5, 4, 0.3, -0.1 );
	mset( Hm, n, 5, 5, 1.5, 1.0 );

	for ( i = 0; i < n; i++ ) {
		mset( Zm, n, i, i, 1.0, 0.0 );
	}

	Sv = reinterpret( S, 0 );
	Sv[ 0 ] = 3.0; Sv[ 1 ] = 1.0;
	Sv[ 2 ] = 2.0; Sv[ 3 ] = -0.5;

	nu = 2 * nshfts + 1;
	Um = { data: new Complex128Array( nu * nu ), s1: 1, s2: nu, offset: 0 };
	WVm = { data: new Complex128Array( n * nu ), s1: 1, s2: n, offset: 0 };
	WHm = { data: new Complex128Array( nu * n ), s1: 1, s2: nu, offset: 0 };

	zlaqr5( true, true, 0, n, 1, n, nshfts,
		S, 1, 0,
		Hm.data, Hm.s1, Hm.s2, Hm.offset,
		1, n,
		Zm.data, Zm.s1, Zm.s2, Zm.offset,
		Vm.data, Vm.s1, Vm.s2, Vm.offset,
		Um.data, Um.s1, Um.s2, Um.offset,
		n,
		WVm.data, WVm.s1, WVm.s2, WVm.offset,
		n,
		WHm.data, WHm.s1, WHm.s2, WHm.offset
	);

	assertArrayClose( getFlat( Hm, n ), tc.H, 1e-12, 'H' );
	assertArrayClose( getFlat( Zm, n ), tc.Z, 1e-12, 'Z' );
});

test( 'zlaqr5: 8x8 with 4 shifts', function t() {
	var tc = findCase( '8x8_4shifts' );
	var n = 8;
	var nshfts = 4;
	var Hm = makeMatrix( n );
	var Zm = makeMatrix( n );
	var Vm = makeMatrix( 3 );
	var Um;
	var WVm;
	var WHm;
	var S = new Complex128Array( nshfts );
	var Sv;
	var nu;
	var i;

	mset( Hm, n, 0, 0, 8.0, 0.5 );
	mset( Hm, n, 0, 1, 1.0, -0.5 );
	mset( Hm, n, 0, 2, 0.5, 0.0 );
	mset( Hm, n, 0, 3, 0.25, 0.1 );
	mset( Hm, n, 1, 0, 1.0, 0.0 );
	mset( Hm, n, 1, 1, 7.0, -0.5 );
	mset( Hm, n, 1, 2, 1.0, 0.5 );
	mset( Hm, n, 1, 3, 0.5, 0.0 );
	mset( Hm, n, 1, 4, 0.25, 0.0 );
	mset( Hm, n, 2, 1, 0.8, 0.1 );
	mset( Hm, n, 2, 2, 6.0, 1.0 );
	mset( Hm, n, 2, 3, 1.0, -0.3 );
	mset( Hm, n, 2, 4, 0.5, 0.0 );
	mset( Hm, n, 2, 5, 0.2, 0.0 );
	mset( Hm, n, 3, 2, 0.7, -0.2 );
	mset( Hm, n, 3, 3, 5.0, -1.0 );
	mset( Hm, n, 3, 4, 1.0, 0.5 );
	mset( Hm, n, 3, 5, 0.4, 0.0 );
	mset( Hm, n, 3, 6, 0.1, 0.0 );
	mset( Hm, n, 4, 3, 0.6, 0.1 );
	mset( Hm, n, 4, 4, 4.0, 0.0 );
	mset( Hm, n, 4, 5, 1.0, -0.5 );
	mset( Hm, n, 4, 6, 0.3, 0.0 );
	mset( Hm, n, 4, 7, 0.1, 0.0 );
	mset( Hm, n, 5, 4, 0.5, -0.1 );
	mset( Hm, n, 5, 5, 3.0, 0.5 );
	mset( Hm, n, 5, 6, 1.0, 0.5 );
	mset( Hm, n, 5, 7, 0.2, 0.0 );
	mset( Hm, n, 6, 5, 0.4, 0.05 );
	mset( Hm, n, 6, 6, 2.0, -0.5 );
	mset( Hm, n, 6, 7, 1.0, -0.5 );
	mset( Hm, n, 7, 6, 0.3, -0.1 );
	mset( Hm, n, 7, 7, 1.0, 1.0 );

	for ( i = 0; i < n; i++ ) {
		mset( Zm, n, i, i, 1.0, 0.0 );
	}

	Sv = reinterpret( S, 0 );
	Sv[ 0 ] = 4.5; Sv[ 1 ] = 0.5;
	Sv[ 2 ] = 3.5; Sv[ 3 ] = -0.5;
	Sv[ 4 ] = 2.5; Sv[ 5 ] = 1.0;
	Sv[ 6 ] = 1.5; Sv[ 7 ] = -1.0;

	nu = 2 * nshfts + 1;
	Um = { data: new Complex128Array( nu * nu ), s1: 1, s2: nu, offset: 0 };
	WVm = { data: new Complex128Array( n * nu ), s1: 1, s2: n, offset: 0 };
	WHm = { data: new Complex128Array( nu * n ), s1: 1, s2: nu, offset: 0 };

	zlaqr5( true, true, 0, n, 1, n, nshfts,
		S, 1, 0,
		Hm.data, Hm.s1, Hm.s2, Hm.offset,
		1, n,
		Zm.data, Zm.s1, Zm.s2, Zm.offset,
		Vm.data, Vm.s1, Vm.s2, Vm.offset,
		Um.data, Um.s1, Um.s2, Um.offset,
		n,
		WVm.data, WVm.s1, WVm.s2, WVm.offset,
		n,
		WHm.data, WHm.s1, WHm.s2, WHm.offset
	);

	assertArrayClose( getFlat( Hm, n ), tc.H, 1e-12, 'H' );
	assertArrayClose( getFlat( Zm, n ), tc.Z, 1e-12, 'Z' );
});

test( 'zlaqr5: 6x6 partial sweep (ktop=2, kbot=5)', function t() {
	var tc = findCase( '6x6_partial_sweep' );
	var n = 6;
	var nshfts = 2;
	var Hm = makeMatrix( n );
	var Zm = makeMatrix( n );
	var Vm = makeMatrix( 3 );
	var Um;
	var WVm;
	var WHm;
	var S = new Complex128Array( nshfts );
	var Sv;
	var nu;
	var i;

	mset( Hm, n, 0, 0, 10.0, 0.0 );
	mset( Hm, n, 0, 1, 1.0, 0.5 );
	mset( Hm, n, 0, 2, 0.5, 0.0 );
	mset( Hm, n, 1, 1, 5.0, 1.0 );
	mset( Hm, n, 1, 2, 2.0, -0.5 );
	mset( Hm, n, 1, 3, 1.0, 0.0 );
	mset( Hm, n, 1, 4, 0.5, 0.2 );
	mset( Hm, n, 2, 1, 1.0, 0.0 );
	mset( Hm, n, 2, 2, 4.0, -1.0 );
	mset( Hm, n, 2, 3, 1.5, 0.5 );
	mset( Hm, n, 2, 4, 0.5, 0.0 );
	mset( Hm, n, 3, 2, 0.8, 0.2 );
	mset( Hm, n, 3, 3, 3.0, 0.5 );
	mset( Hm, n, 3, 4, 1.0, -0.5 );
	mset( Hm, n, 4, 3, 0.6, -0.1 );
	mset( Hm, n, 4, 4, 2.0, 0.0 );
	mset( Hm, n, 4, 5, 0.5, 0.5 );
	mset( Hm, n, 5, 5, -1.0, 2.0 );

	for ( i = 0; i < n; i++ ) {
		mset( Zm, n, i, i, 1.0, 0.0 );
	}

	Sv = reinterpret( S, 0 );
	Sv[ 0 ] = 3.0; Sv[ 1 ] = 0.5;
	Sv[ 2 ] = 2.5; Sv[ 3 ] = -0.5;

	nu = 2 * nshfts + 1;
	Um = { data: new Complex128Array( nu * nu ), s1: 1, s2: nu, offset: 0 };
	WVm = { data: new Complex128Array( n * nu ), s1: 1, s2: n, offset: 0 };
	WHm = { data: new Complex128Array( nu * n ), s1: 1, s2: nu, offset: 0 };

	zlaqr5( true, true, 0, n, 2, 5, nshfts,
		S, 1, 0,
		Hm.data, Hm.s1, Hm.s2, Hm.offset,
		1, n,
		Zm.data, Zm.s1, Zm.s2, Zm.offset,
		Vm.data, Vm.s1, Vm.s2, Vm.offset,
		Um.data, Um.s1, Um.s2, Um.offset,
		n,
		WVm.data, WVm.s1, WVm.s2, WVm.offset,
		n,
		WHm.data, WHm.s1, WHm.s2, WHm.offset
	);

	assertArrayClose( getFlat( Hm, n ), tc.H, 1e-12, 'H' );
	assertArrayClose( getFlat( Zm, n ), tc.Z, 1e-12, 'Z' );
});
