'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var zlaqr3 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zlaqr3.jsonl' ), 'utf8' ).trim().split( '\n' );
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

function buildHess8( Hm ) {
	var n = 8;
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
}


// TESTS //

test( 'zlaqr3: main export is a function', function t() {
	assert.strictEqual( typeof zlaqr3, 'function' );
});

test( 'zlaqr3: 8x8 with NW=3', function t() {
	var tc = findCase( '8x8_nw3' );
	var n = 8;
	var nw = 3;
	var Hm = makeMatrix( n );
	var Zm = makeMatrix( n );
	var Vm = makeMatrix( nw );
	var Tm = makeMatrix( nw );
	var WVm = { data: new Complex128Array( n * nw ), s1: 1, s2: n, offset: 0 };
	var SH = new Complex128Array( n );
	var WORK = new Complex128Array( n * n );
	var result;
	var i;

	buildHess8( Hm );

	for ( i = 0; i < n; i++ ) {
		mset( Zm, n, i, i, 1.0, 0.0 );
	}

	result = zlaqr3( true, true, n, 1, 8, nw,
		Hm.data, Hm.s1, Hm.s2, Hm.offset,
		1, 8,
		Zm.data, Zm.s1, Zm.s2, Zm.offset,
		0, 0,
		SH, 1, 0,
		Vm.data, Vm.s1, Vm.s2, Vm.offset,
		nw,
		Tm.data, Tm.s1, Tm.s2, Tm.offset,
		n,
		WVm.data, WVm.s1, WVm.s2, WVm.offset,
		WORK, 1, 0, n * n
	);

	assert.equal( result.ns, tc.ns );
	assert.equal( result.nd, tc.nd );
	assertArrayClose( getFlat( Hm ), tc.H, 1e-10, 'H' );
	assertArrayClose( getFlat( Zm ), tc.Z, 1e-10, 'Z' );
	assertArrayClose( Array.from( reinterpret( SH, 0 ) ).slice( 0, 2 * n ), tc.SH, 1e-10, 'SH' );
});

test( 'zlaqr3: 8x8 NW=4, no Schur form', function t() {
	var tc = findCase( '8x8_nw4_no_schur' );
	var n = 8;
	var nw = 4;
	var Hm = makeMatrix( n );
	var Zm = makeMatrix( n );
	var Vm = makeMatrix( nw );
	var Tm = makeMatrix( nw );
	var WVm = { data: new Complex128Array( n * nw ), s1: 1, s2: n, offset: 0 };
	var SH = new Complex128Array( n );
	var WORK = new Complex128Array( n * n );
	var result;

	buildHess8( Hm );

	result = zlaqr3( false, false, n, 1, 8, nw,
		Hm.data, Hm.s1, Hm.s2, Hm.offset,
		1, 8,
		Zm.data, Zm.s1, Zm.s2, Zm.offset,
		0, 0,
		SH, 1, 0,
		Vm.data, Vm.s1, Vm.s2, Vm.offset,
		nw,
		Tm.data, Tm.s1, Tm.s2, Tm.offset,
		n,
		WVm.data, WVm.s1, WVm.s2, WVm.offset,
		WORK, 1, 0, n * n
	);

	assert.equal( result.ns, tc.ns );
	assert.equal( result.nd, tc.nd );
	assertArrayClose( Array.from( reinterpret( SH, 0 ) ).slice( 0, 2 * n ), tc.SH, 1e-10, 'SH' );
});
