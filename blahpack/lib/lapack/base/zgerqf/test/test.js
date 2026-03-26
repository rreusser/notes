

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zgerqf = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zgerqf.jsonl' ), 'utf8' ).trim().split( '\n' );
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

/**
* Run zgerqf on a packed complex column-major matrix.
*
* @param {number} M - rows
* @param {number} N - cols
* @param {Float64Array} aFlat - interleaved re/im column-major entries (length 2*M*N)
* @returns {Object} result with A (Float64Array), TAU (Float64Array), info
*/
function runZgerqf( M, N, aFlat ) {
	var WORK = new Complex128Array( Math.max( 1, M * 64 ) );
	var TAU = new Complex128Array( Math.max( 1, Math.min( M, N ) ) );
	var A = new Complex128Array( Math.max( 1, M * N ) );
	var Av = reinterpret( A, 0 );
	var info;
	var i;

	// Copy input into A
	for ( i = 0; i < aFlat.length; i++ ) {
		Av[ i ] = aFlat[ i ];
	}

	info = zgerqf( M, N, A, 1, M, 0, TAU, 1, 0, WORK, 1, 0, M * 64 );

	return {
		A: Array.from( Av ).slice( 0, 2 * M * N ),
		TAU: Array.from( reinterpret( TAU, 0 ) ).slice( 0, 2 * Math.min( M, N ) ),
		info: info
	};
}


// TESTS //

test( 'zgerqf: 3x4 (M < N)', function t() {
	var tc = findCase( '3x4' );
	// Column-major interleaved re/im: each column has M=3 complex entries
	var aFlat = [
		2, 1, 1, 0, 3, -1,       // col 0
		1, 2, 4, 1, 2, 0,        // col 1
		3, 0, 2, -1, 5, 2,       // col 2
		1, 1, 3, 0, 2, -2        // col 3
	];
	var res = runZgerqf( 3, 4, aFlat );
	assert.equal( res.info, tc.info );
	assertArrayClose( res.A, tc.a, 1e-14, 'a' );
	assertArrayClose( res.TAU, tc.tau, 1e-14, 'tau' );
});

test( 'zgerqf: 4x3 (M > N)', function t() {
	var tc = findCase( '4x3' );
	// Column-major interleaved re/im: each column has M=4 complex entries
	var aFlat = [
		2, 1, 1, -1, 3, 0, 1, 2,    // col 0
		1, 0, 4, 1, 2, -1, 3, 0,    // col 1
		3, 1, 2, 0, 5, -2, 1, 1     // col 2
	];
	var res = runZgerqf( 4, 3, aFlat );
	assert.equal( res.info, tc.info );
	assertArrayClose( res.A, tc.a, 1e-14, 'a' );
	assertArrayClose( res.TAU, tc.tau, 1e-14, 'tau' );
});

test( 'zgerqf: 3x3 (square)', function t() {
	var tc = findCase( '3x3' );
	// Column-major interleaved re/im: each column has M=3 complex entries
	var aFlat = [
		4, 1, 1, 0, 2, -1,      // col 0
		1, -1, 3, 2, 1, 0,      // col 1
		2, 0, 1, 1, 5, -2       // col 2
	];
	var res = runZgerqf( 3, 3, aFlat );
	assert.equal( res.info, tc.info );
	assertArrayClose( res.A, tc.a, 1e-14, 'a' );
	assertArrayClose( res.TAU, tc.tau, 1e-14, 'tau' );
});

test( 'zgerqf: 1x1', function t() {
	var tc = findCase( '1x1' );
	var aFlat = [ 5, 3 ];
	var res = runZgerqf( 1, 1, aFlat );
	assert.equal( res.info, tc.info );
	assertArrayClose( res.A, tc.a, 1e-14, 'a' );
	assertArrayClose( res.TAU, tc.tau, 1e-14, 'tau' );
});

test( 'zgerqf: M=0 (quick return)', function t() {
	var tc = findCase( 'm_zero' );
	var A = new Complex128Array( 1 );
	var TAU = new Complex128Array( 1 );
	var WORK = new Complex128Array( 1 );
	var info = zgerqf( 0, 3, A, 1, 1, 0, TAU, 1, 0, WORK, 1, 0, 1 );
	assert.equal( info, tc.info );
});

test( 'zgerqf: N=0 (quick return)', function t() {
	var tc = findCase( 'n_zero' );
	var A = new Complex128Array( 1 );
	var TAU = new Complex128Array( 1 );
	var WORK = new Complex128Array( 1 );
	var info = zgerqf( 3, 0, A, 1, 3, 0, TAU, 1, 0, WORK, 1, 0, 1 );
	assert.equal( info, tc.info );
});

test( 'zgerqf: 2x5 (wide)', function t() {
	var tc = findCase( '2x5' );
	var aFlat = [
		1, 0, 6, -1,
		2, 1, 7, 0,
		3, -1, 8, 2,
		4, 0, 9, -1,
		5, 1, 10, 0
	];
	var res = runZgerqf( 2, 5, aFlat );
	assert.equal( res.info, tc.info );
	assertArrayClose( res.A, tc.a, 1e-14, 'a' );
	assertArrayClose( res.TAU, tc.tau, 1e-14, 'tau' );
});

test( 'zgerqf: 40x40 (blocked path)', function t() {
	var N = 40;
	var aFlat = [];
	var i;
	var j;
	var re;
	var im;

	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < N; i++ ) {
			re = ( ( i + 1 ) * 7 + ( j + 1 ) * 13 ) % 97 / 97.0;
			im = ( ( i + 1 ) * 3 + ( j + 1 ) * 11 ) % 89 / 89.0;
			aFlat.push( re, im );
		}
	}
	var res = runZgerqf( N, N, aFlat );
	assert.equal( res.info, 0 );

	// Verify TAU has nonzero entries
	var tauNonZero = 0;
	for ( i = 0; i < res.TAU.length; i += 2 ) {
		if ( res.TAU[ i ] !== 0.0 || res.TAU[ i + 1 ] !== 0.0 ) {
			tauNonZero += 1;
		}
	}
	assert.ok( tauNonZero > 0, 'TAU should have nonzero entries' );
});
