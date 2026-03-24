'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var path = require( 'path' );
var dsyconv = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dsyconv.jsonl' ), 'utf8' ).trim().split( '\n' );
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
* Convert Fortran 1-based IPIV to JS 0-based IPIV.
* Positive values: subtract 1 (e.g. 2 -> 1).
* Negative values: use bitwise NOT encoding for 0-based index
*   Fortran -p means 1-based row p, so 0-based row is p-1, encoded as ~(p-1).
*   Since ~(p-1) = -p, the encoding is the same numeric value as Fortran.
*
* @private
* @param {Array} ipivFortran - Fortran 1-based IPIV array
* @returns {Int32Array} 0-based IPIV
*/
function convertIPIV( ipivFortran ) {
	var out = new Int32Array( ipivFortran.length );
	var i;
	for ( i = 0; i < ipivFortran.length; i++ ) {
		if ( ipivFortran[ i ] >= 0 ) {
			out[ i ] = ipivFortran[ i ] - 1;
		} else {
			// Fortran -p (1-based row p) -> JS ~(p-1) = -p (same value)
			out[ i ] = ipivFortran[ i ];
		}
	}
	return out;
}


// TESTS //

test( 'dsyconv: upper_convert (all 1x1 pivots)', function t() {
	var tc = findCase( 'upper_convert' );
	var N = 4;
	var A = new Float64Array( tc.a_factored );
	var IPIV = convertIPIV( tc.ipiv_trf );
	var E = new Float64Array( N );
	var info;

	info = dsyconv( 'U', 'C', N, A, 1, N, 0, IPIV, 1, 0, E, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertArrayClose( Array.from( A ), tc.a_converted, 1e-14, 'a_converted' );
	assertArrayClose( Array.from( E ), tc.e, 1e-14, 'e' );
});

test( 'dsyconv: upper_revert (all 1x1 pivots)', function t() {
	var tcConv = findCase( 'upper_convert' );
	var tcRev = findCase( 'upper_revert' );
	var N = 4;
	var A = new Float64Array( tcConv.a_converted );
	var IPIV = convertIPIV( tcConv.ipiv_trf );
	var E = new Float64Array( tcConv.e );
	var info;

	info = dsyconv( 'U', 'R', N, A, 1, N, 0, IPIV, 1, 0, E, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertArrayClose( Array.from( A ), tcRev.a_reverted, 1e-14, 'a_reverted' );
});

test( 'dsyconv: lower_convert (all 1x1 pivots)', function t() {
	var tc = findCase( 'lower_convert' );
	var N = 4;
	var A = new Float64Array( tc.a_factored );
	var IPIV = convertIPIV( tc.ipiv_trf );
	var E = new Float64Array( N );
	var info;

	info = dsyconv( 'L', 'C', N, A, 1, N, 0, IPIV, 1, 0, E, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertArrayClose( Array.from( A ), tc.a_converted, 1e-14, 'a_converted' );
	assertArrayClose( Array.from( E ), tc.e, 1e-14, 'e' );
});

test( 'dsyconv: lower_revert (all 1x1 pivots)', function t() {
	var tcConv = findCase( 'lower_convert' );
	var tcRev = findCase( 'lower_revert' );
	var N = 4;
	var A = new Float64Array( tcConv.a_converted );
	var IPIV = convertIPIV( tcConv.ipiv_trf );
	var E = new Float64Array( tcConv.e );
	var info;

	info = dsyconv( 'L', 'R', N, A, 1, N, 0, IPIV, 1, 0, E, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertArrayClose( Array.from( A ), tcRev.a_reverted, 1e-14, 'a_reverted' );
});

test( 'dsyconv: n1_upper', function t() {
	var tc = findCase( 'n1_upper' );
	var A = new Float64Array( tc.a_factored );
	var IPIV = convertIPIV( tc.ipiv );
	var E = new Float64Array( 1 );
	var info;

	info = dsyconv( 'U', 'C', 1, A, 1, 1, 0, IPIV, 1, 0, E, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertArrayClose( Array.from( A ), tc.a_converted, 1e-14, 'a_converted' );
	assertArrayClose( Array.from( E ), tc.e, 1e-14, 'e' );
});

test( 'dsyconv: n1_lower', function t() {
	var tc = findCase( 'n1_lower' );
	var A = new Float64Array( tc.a_factored );
	var IPIV = convertIPIV( tc.ipiv );
	var E = new Float64Array( 1 );
	var info;

	info = dsyconv( 'L', 'C', 1, A, 1, 1, 0, IPIV, 1, 0, E, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertArrayClose( Array.from( A ), tc.a_converted, 1e-14, 'a_converted' );
	assertArrayClose( Array.from( E ), tc.e, 1e-14, 'e' );
});

test( 'dsyconv: upper_2x2_convert (with 2x2 pivots)', function t() {
	var tc = findCase( 'upper_2x2_convert' );
	var N = 4;
	var A = new Float64Array( tc.a_factored );
	var IPIV = convertIPIV( tc.ipiv_trf );
	var E = new Float64Array( N );
	var info;

	info = dsyconv( 'U', 'C', N, A, 1, N, 0, IPIV, 1, 0, E, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertArrayClose( Array.from( A ), tc.a_converted, 1e-14, 'a_converted' );
	assertArrayClose( Array.from( E ), tc.e, 1e-14, 'e' );
});

test( 'dsyconv: upper_2x2_revert (with 2x2 pivots)', function t() {
	var tcConv = findCase( 'upper_2x2_convert' );
	var tcRev = findCase( 'upper_2x2_revert' );
	var N = 4;
	var A = new Float64Array( tcConv.a_converted );
	var IPIV = convertIPIV( tcConv.ipiv_trf );
	var E = new Float64Array( tcConv.e );
	var info;

	info = dsyconv( 'U', 'R', N, A, 1, N, 0, IPIV, 1, 0, E, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertArrayClose( Array.from( A ), tcRev.a_reverted, 1e-14, 'a_reverted' );
});

test( 'dsyconv: lower_2x2_convert (with 2x2 pivots)', function t() {
	var tc = findCase( 'lower_2x2_convert' );
	var N = 4;
	var A = new Float64Array( tc.a_factored );
	var IPIV = convertIPIV( tc.ipiv_trf );
	var E = new Float64Array( N );
	var info;

	info = dsyconv( 'L', 'C', N, A, 1, N, 0, IPIV, 1, 0, E, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertArrayClose( Array.from( A ), tc.a_converted, 1e-14, 'a_converted' );
	assertArrayClose( Array.from( E ), tc.e, 1e-14, 'e' );
});

test( 'dsyconv: lower_2x2_revert (with 2x2 pivots)', function t() {
	var tcConv = findCase( 'lower_2x2_convert' );
	var tcRev = findCase( 'lower_2x2_revert' );
	var N = 4;
	var A = new Float64Array( tcConv.a_converted );
	var IPIV = convertIPIV( tcConv.ipiv_trf );
	var E = new Float64Array( tcConv.e );
	var info;

	info = dsyconv( 'L', 'R', N, A, 1, N, 0, IPIV, 1, 0, E, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertArrayClose( Array.from( A ), tcRev.a_reverted, 1e-14, 'a_reverted' );
});

test( 'dsyconv: N=0 returns immediately', function t() {
	var A = new Float64Array( 0 );
	var IPIV = new Int32Array( 0 );
	var E = new Float64Array( 0 );
	var info;

	info = dsyconv( 'U', 'C', 0, A, 1, 1, 0, IPIV, 1, 0, E, 1, 0 );
	assert.equal( info, 0, 'info upper convert' );

	info = dsyconv( 'L', 'R', 0, A, 1, 1, 0, IPIV, 1, 0, E, 1, 0 );
	assert.equal( info, 0, 'info lower revert' );
});

test( 'dsyconv: round-trip upper convert then revert restores A', function t() {
	var tc = findCase( 'upper_2x2_convert' );
	var N = 4;
	var Aorig = new Float64Array( tc.a_factored );
	var A = new Float64Array( tc.a_factored );
	var IPIV = convertIPIV( tc.ipiv_trf );
	var E = new Float64Array( N );

	dsyconv( 'U', 'C', N, A, 1, N, 0, IPIV, 1, 0, E, 1, 0 );
	dsyconv( 'U', 'R', N, A, 1, N, 0, IPIV, 1, 0, E, 1, 0 );
	assertArrayClose( Array.from( A ), Array.from( Aorig ), 1e-14, 'round-trip' );
});

test( 'dsyconv: round-trip lower convert then revert restores A', function t() {
	var tc = findCase( 'lower_2x2_convert' );
	var N = 4;
	var Aorig = new Float64Array( tc.a_factored );
	var A = new Float64Array( tc.a_factored );
	var IPIV = convertIPIV( tc.ipiv_trf );
	var E = new Float64Array( N );

	dsyconv( 'L', 'C', N, A, 1, N, 0, IPIV, 1, 0, E, 1, 0 );
	dsyconv( 'L', 'R', N, A, 1, N, 0, IPIV, 1, 0, E, 1, 0 );
	assertArrayClose( Array.from( A ), Array.from( Aorig ), 1e-14, 'round-trip' );
});
