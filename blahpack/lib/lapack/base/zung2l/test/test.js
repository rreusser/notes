'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var zung2l = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zung2l.jsonl' ), 'utf8' ).trim().split( '\n' );
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
* Extract NxN portion from LDA-based fixture data (LDA=NMAX=4).
*/
function extractNxN( data, M, N, LDA ) {
	var result = [];
	var j;
	var i;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			result.push( data[ j * LDA * 2 + i * 2 ] );
			result.push( data[ j * LDA * 2 + i * 2 + 1 ] );
		}
	}
	return result;
}


// TESTS //

test( 'zung2l: 3x3 full Q (M=N=K=3)', function t() {
	var tc = findCase( 'zung2l_3x3' );
	var Adata = new Float64Array([
		// col 0
		0.5, 0.5,  1.0, 0.0,  0.0, 0.0,
		// col 1
		0.0, 1.0,  0.5, -0.5,  0.0, 0.0,
		// col 2
		1.0, 0.0,  0.0, 0.5,  0.3, 0.0
	]);
	var A = new Complex128Array( Adata.buffer.slice(0) );
	var TAU = new Complex128Array( new Float64Array([ 1.2, 0.1, 0.8, -0.2, 1.5, 0.3 ]).buffer );
	var WORK = new Complex128Array( 10 );

	var info = zung2l( 3, 3, 3, A, 1, 3, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.equal( info, 0, 'info=0' );

	var expected = extractNxN( tc.Q, 3, 3, 4 );
	assertArrayClose( Array.from( new Float64Array(A.buffer) ), expected, 1e-13, 'Q' );
});

test( 'zung2l: 4x3 rectangular (M=4, N=3, K=2)', function t() {
	var tc = findCase( 'zung2l_4x3' );
	var Adata = new Float64Array([
		// col 0 (not a reflector since K=2)
		0.0, 0.0,  0.0, 0.0,  0.0, 0.0,  0.0, 0.0,
		// col 1 (reflector H(1))
		0.3, 0.4,  0.5, -0.1,  1.0, 0.0,  0.0, 0.0,
		// col 2 (reflector H(2))
		0.2, 0.1,  0.4, -0.3,  0.0, 0.6,  0.7, 0.0
	]);
	var A = new Complex128Array( Adata.buffer.slice(0) );
	var TAU = new Complex128Array( new Float64Array([ 1.1, 0.2, 0.9, -0.1 ]).buffer );
	var WORK = new Complex128Array( 10 );

	var info = zung2l( 4, 3, 2, A, 1, 4, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.equal( info, 0, 'info=0' );

	var expected = extractNxN( tc.Q, 4, 3, 4 );
	assertArrayClose( Array.from( new Float64Array(A.buffer) ), expected, 1e-13, 'Q' );
});

test( 'zung2l: K=0 (identity matrix)', function t() {
	var Adata = new Float64Array([
		9.0, 9.0,  0.0, 0.0,  0.0, 0.0,
		0.0, 0.0,  9.0, 9.0,  0.0, 0.0,
		0.0, 0.0,  0.0, 0.0,  9.0, 9.0
	]);
	var A = new Complex128Array( Adata.buffer.slice(0) );
	var TAU = new Complex128Array( 1 );
	var WORK = new Complex128Array( 10 );

	var info = zung2l( 3, 3, 0, A, 1, 3, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.equal( info, 0, 'info=0' );

	var Q = new Float64Array( A.buffer );
	// Should be identity
	assertArrayClose( Array.from(Q), [
		1, 0,  0, 0,  0, 0,
		0, 0,  1, 0,  0, 0,
		0, 0,  0, 0,  1, 0
	], 1e-15, 'identity' );
});

test( 'zung2l: N=0', function t() {
	var A = new Complex128Array( 0 );
	var TAU = new Complex128Array( 0 );
	var WORK = new Complex128Array( 1 );

	var info = zung2l( 3, 0, 0, A, 1, 3, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.equal( info, 0, 'info=0' );
});
