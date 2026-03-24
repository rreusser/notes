

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dgees = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dgees.jsonl' ), 'utf8' ).trim().split( '\n' );
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

function fromFortranColMajor( arr, N, LDA ) {
	var out = new Float64Array( N * N );
	var i;
	var j;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < N; i++ ) {
			out[ i + j * N ] = arr[ i + j * LDA ];
		}
	}
	return out;
}

function selectPosReal( wr ) {
	return wr > 0.0;
}

function selectNone() {
	return false;
}

function runDgees( jobvs, sort, select, N, Adata ) {
	var A = new Float64Array( Adata );
	var VS = new Float64Array( N * N );
	var WR = new Float64Array( N );
	var WI = new Float64Array( N );
	var sdim = new Float64Array( 1 );
	var WORK = new Float64Array( Math.max( 200, 10 * N ) );
	var BWORK = new Uint8Array( N );
	var lwork = WORK.length;

	var info = dgees( jobvs, sort, select, N, A, 1, N, 0, sdim, WR, 1, 0, WI, 1, 0, VS, 1, N, 0, WORK, 1, 0, lwork, BWORK, 1, 0 );

	return { 'info': info, 'A': A, 'VS': VS, 'WR': WR, 'WI': WI, 'sdim': sdim[ 0 ] };
}


// TESTS //

test( 'dgees: NN basic 3x3 tri', function t() {
	var tc = findCase( 'NN basic 3x3 tri' );
	var N = 3;
	var A = fromFortranColMajor( [
		2, 0, 0, 0,
		1, 3, 0, 0,
		0, 1, 4, 0,
		0, 0, 0, 0
	], N, 4 );
	var res = runDgees( 'N', 'N', selectNone, N, A );

	assert.equal( res.info, tc.info );
	assert.equal( res.sdim, tc.SDIM );
	var sortedWR = Array.from( res.WR ).sort();
	assertArrayClose( sortedWR, [ 2.0, 3.0, 4.0 ], 1e-12, 'WR' );
});

test( 'dgees: VN 3x3 general', function t() {
	var tc = findCase( 'VN 3x3 general' );
	var N = 3;
	var A = fromFortranColMajor( [
		1, 4, 7, 0,
		2, 5, 8, 0,
		3, 6, 0, 0,
		0, 0, 0, 0
	], N, 4 );
	var res = runDgees( 'V', 'N', selectNone, N, A );

	assert.equal( res.info, tc.info );
	assert.equal( res.sdim, tc.SDIM );

	var expectedWR = tc.WR.slice( 0, N ).sort( function( a, b ) { return a - b; } );
	var actualWR = Array.from( res.WR ).sort( function( a, b ) { return a - b; } );
	assertArrayClose( actualWR, expectedWR, 1e-12, 'WR' );
});

test( 'dgees: VS select positive real', function t() {
	var tc = findCase( 'VS select positive real' );
	var N = 3;
	var A = fromFortranColMajor( [
		0, -1, 0, 0,
		1, 0, 0, 0,
		0, 0, 2, 0,
		0, 0, 0, 0
	], N, 4 );
	var res = runDgees( 'V', 'S', selectPosReal, N, A );

	assert.equal( res.info, tc.info );
	assert.equal( res.sdim, tc.SDIM );
	assertClose( res.WR[ 0 ], 2.0, 1e-12, 'WR[0]' );
});

test( 'dgees: N=0', function t() {
	var tc = findCase( 'N=0' );
	var A = new Float64Array( 0 );
	var VS = new Float64Array( 0 );
	var WR = new Float64Array( 0 );
	var WI = new Float64Array( 0 );
	var sdim = new Float64Array( 1 );
	var WORK = new Float64Array( 200 );
	var BWORK = new Uint8Array( 0 );

	var info = dgees( 'N', 'N', selectNone, 0, A, 1, 0, 0, sdim, WR, 1, 0, WI, 1, 0, VS, 1, 0, 0, WORK, 1, 0, 200, BWORK, 1, 0 );

	assert.equal( info, tc.info );
	assert.equal( sdim[ 0 ], tc.SDIM );
});

test( 'dgees: N=1', function t() {
	var tc = findCase( 'N=1' );
	var N = 1;
	var A = new Float64Array([ 5.0 ]);
	var res = runDgees( 'V', 'N', selectNone, N, A );

	assert.equal( res.info, tc.info );
	assertClose( res.WR[ 0 ], 5.0, 1e-12, 'WR[0]' );
	assertClose( res.WI[ 0 ], 0.0, 1e-12, 'WI[0]' );
	assertClose( res.VS[ 0 ], 1.0, 1e-12, 'VS[0]' );
});

test( 'dgees: N=2 complex eigs', function t() {
	var tc = findCase( 'N=2 complex eigs' );
	var N = 2;
	var A = new Float64Array([
		0.0, -1.0,
		1.0, 0.0
	]);
	var res = runDgees( 'V', 'N', selectNone, N, A );

	assert.equal( res.info, tc.info );
	assertClose( res.WR[ 0 ], 0.0, 1e-12, 'WR[0]' );
	assertClose( res.WR[ 1 ], 0.0, 1e-12, 'WR[1]' );
	assertClose( Math.abs( res.WI[ 0 ] ), 1.0, 1e-12, '|WI[0]|' );
	assertClose( Math.abs( res.WI[ 1 ] ), 1.0, 1e-12, '|WI[1]|' );
	assertClose( res.WI[ 0 ] + res.WI[ 1 ], 0.0, 1e-12, 'conjugate' );
});

test( 'dgees: VN 4x4 general', function t() {
	var tc = findCase( 'VN 4x4 general' );
	var N = 4;
	var A = fromFortranColMajor( [
		4, 0, 2, 0,
		1, 3, 1, 0,
		2, 1, 1, 1,
		0, 1, 0, 2
	], N, N );
	var res = runDgees( 'V', 'N', selectNone, N, A );

	assert.equal( res.info, tc.info );

	var expectedWR = tc.WR.slice( 0, N ).sort( function( a, b ) { return a - b; } );
	var actualWR = Array.from( res.WR ).sort( function( a, b ) { return a - b; } );
	assertArrayClose( actualWR, expectedWR, 1e-10, 'WR' );
});

test( 'dgees: verify Schur decomposition A = Z*T*Z^T', function t() {
	var N = 3;
	var Aorig = new Float64Array([
		1, 4, 7,
		2, 5, 8,
		3, 6, 0
	]);
	var A = new Float64Array( Aorig );
	var VS = new Float64Array( N * N );
	var WR = new Float64Array( N );
	var WI = new Float64Array( N );
	var sdim = new Float64Array( 1 );
	var WORK = new Float64Array( 200 );
	var BWORK = new Uint8Array( N );

	var info = dgees( 'V', 'N', selectNone, N, A, 1, N, 0, sdim, WR, 1, 0, WI, 1, 0, VS, 1, N, 0, WORK, 1, 0, 200, BWORK, 1, 0 );
	assert.equal( info, 0 );

	// Verify: Z^T * Aorig * Z ~= T
	var tmp = new Float64Array( N * N );
	var result = new Float64Array( N * N );
	var i;
	var j;
	var k;
	var s;
	for ( i = 0; i < N; i++ ) {
		for ( j = 0; j < N; j++ ) {
			s = 0;
			for ( k = 0; k < N; k++ ) {
				s += VS[ k + i * N ] * Aorig[ k + j * N ];
			}
			tmp[ i + j * N ] = s;
		}
	}
	for ( i = 0; i < N; i++ ) {
		for ( j = 0; j < N; j++ ) {
			s = 0;
			for ( k = 0; k < N; k++ ) {
				s += tmp[ i + k * N ] * VS[ k + j * N ];
			}
			result[ i + j * N ] = s;
		}
	}

	assertArrayClose( Array.from( result ), Array.from( A ), 1e-10, 'Z^T*A*Z = T' );
});
