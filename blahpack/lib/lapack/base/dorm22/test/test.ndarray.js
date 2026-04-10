

/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var dorm22 = require( './../lib/base.js' );
var ndarrayFn = require( './../lib/ndarray.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dorm22.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var relErr;
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		relErr = Math.abs( actual[ i ] - expected[ i ] ) / Math.max( Math.abs( expected[ i ] ), 1.0 );
		assert.ok( relErr <= tol, msg + '[' + i + ']: expected ' + expected[ i ] + ', got ' + actual[ i ] );
	}
}

// Build the 5x5 banded Q from the Fortran test, packed column-major tight.
function buildQ5() {
	var Q = new Float64Array( 25 );
	function set( i, j, v ) { Q[ ((j - 1) * 5) + (i - 1) ] = v; }
	set( 1, 1, 0.5 );   set( 1, 2, -0.3 );
	set( 2, 1, 0.2 );   set( 2, 2, 0.8 );
	set( 3, 1, -0.4 );  set( 3, 2, 0.1 );
	set( 1, 3, 1.1 );
	set( 2, 3, 0.7 );   set( 2, 4, 0.9 );
	set( 3, 3, -0.5 );  set( 3, 4, 0.4 );  set( 3, 5, 1.2 );
	set( 4, 1, 0.6 );   set( 4, 2, -0.2 );
	                    set( 5, 2, 1.3 );
	set( 4, 3, 0.3 );   set( 4, 4, -0.1 ); set( 4, 5, 0.5 );
	set( 5, 3, -0.7 );  set( 5, 4, 0.4 );  set( 5, 5, 0.2 );
	return Q;
}

function buildC( M, N ) {
	var C = new Float64Array( M * N );
	var i;
	var j;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			C[ (j * M) + i ] = ( ( (i + 1) - 2 + (j + 1) ) * 0.25 ) + 0.1;
		}
	}
	return C;
}


// TESTS //

test( 'base is a function', function t() {
	assert.strictEqual( typeof dorm22, 'function', 'is a function' );
});

test( 'ndarray is a function', function t() {
	assert.strictEqual( typeof ndarrayFn, 'function', 'is a function' );
});

test( 'ndarray: unit strides, zero offsets match left_notrans fixture', function t() {
	var WORK = new Float64Array( 200 );
	var tc = findCase( 'left_notrans' );
	var Q = buildQ5();
	var M = 5;
	var N = 4;
	var C = buildC( M, N );
	var info = ndarrayFn( 'left', 'no-transpose', M, N, 3, 2, Q, 1, 5, 0, C, 1, M, 0, WORK, 1, 0, 200 );
	assert.equal( info, 0, 'info' );
	assertArrayClose( Array.from( C ), tc.c, 1e-13, 'c' );
});

test( 'ndarray: unit strides, zero offsets match right_trans fixture', function t() {
	var WORK = new Float64Array( 200 );
	var tc = findCase( 'right_trans' );
	var Q = buildQ5();
	var M = 4;
	var N = 5;
	var C = buildC( M, N );
	var info = ndarrayFn( 'right', 'transpose', M, N, 3, 2, Q, 1, 5, 0, C, 1, M, 0, WORK, 1, 0, 200 );
	assert.equal( info, 0, 'info' );
	assertArrayClose( Array.from( C ), tc.c, 1e-13, 'c' );
});

test( 'ndarray: non-unit strides with interleaved C layout', function t() {
	var strideC2;
	var strideC1;
	var WORK;
	var info;
	var out;
	var tc;
	var Q;
	var C;
	var M;
	var N;
	var i;
	var j;

	tc = findCase( 'left_notrans' );
	Q = buildQ5();
	M = 5;
	N = 4;

	// Lay C into a buffer with row stride 2 and column stride 2*M. Every
	// other slot is padding that must be left untouched by dorm22.
	strideC1 = 2;
	strideC2 = 2 * M;
	C = new Float64Array( strideC2 * N );
	for ( i = 0; i < C.length; i++ ) {
		C[ i ] = -7777.0;
	}
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			C[ (j * strideC2) + (i * strideC1) ] = ( ( (i + 1) - 2 + (j + 1) ) * 0.25 ) + 0.1;
		}
	}
	WORK = new Float64Array( 200 );
	info = ndarrayFn( 'left', 'no-transpose', M, N, 3, 2, Q, 1, 5, 0, C, strideC1, strideC2, 0, WORK, 1, 0, 200 );
	assert.equal( info, 0, 'info' );

	// Extract the logical matrix and compare to the fixture.
	out = [];
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			out.push( C[ (j * strideC2) + (i * strideC1) ] );
		}
	}
	assertArrayClose( out, tc.c, 1e-13, 'c' );

	// Padding slots must be untouched.
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			assert.equal( C[ (j * strideC2) + (i * strideC1) + 1 ], -7777.0, 'pad' );
		}
	}
});

test( 'ndarray: non-zero offsets into padded buffers', function t() {
	var Qbase;
	var Cbase;
	var WORK;
	var info;
	var Qpad;
	var Cpad;
	var out;
	var tc;
	var M;
	var N;
	var i;

	tc = findCase( 'left_trans' );
	M = 5;
	N = 4;

	// Pad Q with 7 leading elements; use offsetQ = 7.
	Qbase = buildQ5();
	Qpad = new Float64Array( Qbase.length + 7 );
	for ( i = 0; i < 7; i++ ) {
		Qpad[ i ] = 1234.5 + i;
	}
	for ( i = 0; i < Qbase.length; i++ ) {
		Qpad[ i + 7 ] = Qbase[ i ];
	}

	// Pad C with 3 leading elements; use offsetC = 3.
	Cbase = buildC( M, N );
	Cpad = new Float64Array( Cbase.length + 3 );
	for ( i = 0; i < 3; i++ ) {
		Cpad[ i ] = -9999.0;
	}
	for ( i = 0; i < Cbase.length; i++ ) {
		Cpad[ i + 3 ] = Cbase[ i ];
	}

	// Pad WORK with 5 leading slots; use offsetWORK = 5.
	WORK = new Float64Array( 205 );
	for ( i = 0; i < 5; i++ ) {
		WORK[ i ] = 42.0;
	}

	info = ndarrayFn( 'left', 'transpose', M, N, 3, 2, Qpad, 1, 5, 7, Cpad, 1, M, 3, WORK, 1, 5, 200 );
	assert.equal( info, 0, 'info' );

	// Extract logical C and compare to the fixture.
	out = [];
	for ( i = 0; i < M * N; i++ ) {
		out.push( Cpad[ i + 3 ] );
	}
	assertArrayClose( out, tc.c, 1e-13, 'c' );

	// Padding in C must be untouched.
	for ( i = 0; i < 3; i++ ) {
		assert.equal( Cpad[ i ], -9999.0, 'C pad' );
	}
	// Padding in Q must be untouched.
	for ( i = 0; i < 7; i++ ) {
		assert.equal( Qpad[ i ], 1234.5 + i, 'Q pad' );
	}
	// Padding in WORK must be untouched.
	for ( i = 0; i < 5; i++ ) {
		assert.equal( WORK[ i ], 42.0, 'WORK pad' );
	}
});
