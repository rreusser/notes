'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dtrexc = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dtrexc.jsonl' ), 'utf8' ).trim().split( '\n' );
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


// TESTS //

test( 'dtrexc: swap 1x1 forward', function t() {
	var tc = findCase( 'swap 1x1 forward' );
	var N = 4;

	// Upper triangular: T(i,j) in column-major, LDT=N
	var T = new Float64Array( N * N );
	T[ 0 + 0*N ] = 4.0; T[ 0 + 1*N ] = 1.0; T[ 0 + 2*N ] = 0.5; T[ 0 + 3*N ] = 0.2;
	T[ 1 + 1*N ] = 3.0; T[ 1 + 2*N ] = 0.8; T[ 1 + 3*N ] = 0.3;
	T[ 2 + 2*N ] = 2.0; T[ 2 + 3*N ] = 0.6;
	T[ 3 + 3*N ] = 1.0;

	var Q = new Float64Array( N * N );
	for ( var i = 0; i < N; i++ ) Q[ i + i*N ] = 1.0;

	var WORK = new Float64Array( N );

	var r = dtrexc( 'V', N, T, 1, N, 0, Q, 1, N, 0, 1, 4, WORK, 1, 0 );

	assert.strictEqual( r.info, tc.info, 'info' );
	assertArrayClose( Array.from( T ), tc.T, 1e-12, 'T' );
	assertArrayClose( Array.from( Q ), tc.Q, 1e-12, 'Q' );
});

test( 'dtrexc: N=1 quick return', function t() {
	var T = new Float64Array([ 5.0 ]);
	var Q = new Float64Array([ 1.0 ]);
	var WORK = new Float64Array( 1 );

	var r = dtrexc( 'V', 1, T, 1, 1, 0, Q, 1, 1, 0, 1, 1, WORK, 1, 0 );
	assert.strictEqual( r.info, 0, 'info' );
});
