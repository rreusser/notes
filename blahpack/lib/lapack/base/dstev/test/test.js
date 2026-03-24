

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var dstev = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dstev.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dstev: eigenvalues_only_5x5 (JOBZ=N)', function t() {
	var tc = findCase( 'eigenvalues_only_5x5' );
	var N = 5;
	var d = new Float64Array( [ 2, 2, 2, 2, 2 ] );
	var e = new Float64Array( [ -1, -1, -1, -1 ] );
	var Z = new Float64Array( 1 );
	var WORK = new Float64Array( 1 );
	var info;

	info = dstev( 'N', N, d, 1, 0, e, 1, 0, Z, 1, 1, 0, WORK, 1, 0 );

	assert.equal( info, tc.info );
	assertArrayClose( Array.from( d ), tc.d, 1e-14, 'd' );
});

test( 'dstev: eigenvectors_5x5 (JOBZ=V)', function t() {
	var tc = findCase( 'eigenvectors_5x5' );
	var N = 5;
	var d = new Float64Array( [ 2, 2, 2, 2, 2 ] );
	var e = new Float64Array( [ -1, -1, -1, -1 ] );
	var Z = new Float64Array( N * N );
	var WORK = new Float64Array( 2 * N - 2 );
	var info;
	var i;

	info = dstev( 'V', N, d, 1, 0, e, 1, 0, Z, 1, N, 0, WORK, 1, 0 );

	assert.equal( info, tc.info );
	assertArrayClose( Array.from( d ), tc.d, 1e-13, 'd' );

	// For eigenvectors, check up to sign flip (each column may be negated)
	for ( i = 0; i < N; i++ ) {
		assertEigenvectorClose( Z, N, i, tc.z, 1e-13, 'z col ' + i );
	}
});

test( 'dstev: eigenvectors_4x4 (JOBZ=V)', function t() {
	var tc = findCase( 'eigenvectors_4x4' );
	var N = 4;
	var d = new Float64Array( [ 4, 1, 3, 2 ] );
	var e = new Float64Array( [ 1, 0.5, 1.5 ] );
	var Z = new Float64Array( N * N );
	var WORK = new Float64Array( 2 * N - 2 );
	var info;
	var i;

	info = dstev( 'V', N, d, 1, 0, e, 1, 0, Z, 1, N, 0, WORK, 1, 0 );

	assert.equal( info, tc.info );
	assertArrayClose( Array.from( d ), tc.d, 1e-13, 'd' );

	for ( i = 0; i < N; i++ ) {
		assertEigenvectorClose( Z, N, i, tc.z, 1e-13, 'z col ' + i );
	}
});

test( 'dstev: n_zero', function t() {
	var tc = findCase( 'n_zero' );
	var d = new Float64Array( 0 );
	var e = new Float64Array( 0 );
	var Z = new Float64Array( 1 );
	var WORK = new Float64Array( 1 );
	var info;

	info = dstev( 'N', 0, d, 1, 0, e, 1, 0, Z, 1, 1, 0, WORK, 1, 0 );

	assert.equal( info, tc.info );
});

test( 'dstev: n_one (JOBZ=V)', function t() {
	var tc = findCase( 'n_one' );
	var d = new Float64Array( [ 7.5 ] );
	var e = new Float64Array( 0 );
	var Z = new Float64Array( 1 );
	var WORK = new Float64Array( 1 );
	var info;

	info = dstev( 'V', 1, d, 1, 0, e, 1, 0, Z, 1, 1, 0, WORK, 1, 0 );

	assert.equal( info, tc.info );
	assertArrayClose( Array.from( d ), tc.d, 1e-14, 'd' );
	assertArrayClose( Array.from( Z ), tc.z, 1e-14, 'z' );
});

test( 'dstev: already_sorted (JOBZ=N, diagonal matrix)', function t() {
	var tc = findCase( 'already_sorted' );
	var N = 4;
	var d = new Float64Array( [ 1, 2, 3, 4 ] );
	var e = new Float64Array( [ 0, 0, 0 ] );
	var Z = new Float64Array( 1 );
	var WORK = new Float64Array( 1 );
	var info;

	info = dstev( 'N', N, d, 1, 0, e, 1, 0, Z, 1, 1, 0, WORK, 1, 0 );

	assert.equal( info, tc.info );
	assertArrayClose( Array.from( d ), tc.d, 1e-14, 'd' );
});


test( 'dstev: very small values trigger scaling path (tnrm < RMIN)', function t() {
	// RMIN ~ 1.4e-146; use entries ~ 1e-148 so dlanst('M',...) < RMIN
	var N = 4;
	var scale = 1e-148;
	var d = new Float64Array( [ 4 * scale, 1 * scale, 3 * scale, 2 * scale ] );
	var e = new Float64Array( [ 1 * scale, 0.5 * scale, 1.5 * scale ] );
	var Z = new Float64Array( N * N );
	var WORK = new Float64Array( 2 * N - 2 );
	var info;

	info = dstev( 'V', N, d, 1, 0, e, 1, 0, Z, 1, N, 0, WORK, 1, 0 );

	assert.equal( info, 0, 'info' );
	// Eigenvalues should be in ascending order and roughly scale * {original eigs}
	for ( var i = 0; i < N - 1; i++ ) {
		assert.ok( d[ i ] <= d[ i + 1 ] + 1e-300, 'eigenvalues sorted: d[' + i + ']=' + d[i] + ' <= d[' + (i+1) + ']=' + d[i+1] );
	}
});

test( 'dstev: very large values trigger scaling path (tnrm > RMAX)', function t() {
	// RMAX ~ 7e+145; use entries ~ 1e+147 so dlanst('M',...) > RMAX
	var N = 4;
	var scale = 1e147;
	var d = new Float64Array( [ 4 * scale, 1 * scale, 3 * scale, 2 * scale ] );
	var e = new Float64Array( [ 1 * scale, 0.5 * scale, 1.5 * scale ] );
	var Z = new Float64Array( N * N );
	var WORK = new Float64Array( 2 * N - 2 );
	var info;

	info = dstev( 'V', N, d, 1, 0, e, 1, 0, Z, 1, N, 0, WORK, 1, 0 );

	assert.equal( info, 0, 'info' );
	for ( var i = 0; i < N - 1; i++ ) {
		assert.ok( d[ i ] <= d[ i + 1 ] * ( 1 + 1e-10 ), 'eigenvalues sorted: d[' + i + ']=' + d[i] + ' <= d[' + (i+1) + ']=' + d[i+1] );
	}
});

test( 'dstev: JOBZ=V with larger 8x8 matrix', function t() {
	var N = 8;
	var d = new Float64Array( [ 10, 5, 8, 3, 7, 2, 6, 1 ] );
	var e = new Float64Array( [ 1, 1, 1, 1, 1, 1, 1 ] );
	var Z = new Float64Array( N * N );
	var WORK = new Float64Array( 2 * N - 2 );
	var info;
	var i;

	info = dstev( 'V', N, d, 1, 0, e, 1, 0, Z, 1, N, 0, WORK, 1, 0 );

	assert.equal( info, 0, 'info' );
	// Verify eigenvalues are sorted
	for ( i = 0; i < N - 1; i++ ) {
		assert.ok( d[ i ] <= d[ i + 1 ] + 1e-10, 'eigenvalues sorted' );
	}

	// Verify orthogonality: Z^T * Z = I
	var dot;
	var j;
	var k;
	for ( i = 0; i < N; i++ ) {
		for ( j = i; j < N; j++ ) {
			dot = 0.0;
			for ( k = 0; k < N; k++ ) {
				dot += Z[ i * N + k ] * Z[ j * N + k ];
			}
			if ( i === j ) {
				assertClose( dot, 1.0, 1e-12, 'Z^T*Z diagonal(' + i + ')' );
			} else {
				assertClose( dot, 0.0, 1e-12, 'Z^T*Z off-diagonal(' + i + ',' + j + ')' );
			}
		}
	}
});

test( 'dstev: eigenvalues only with scaling (JOBZ=N, small values)', function t() {
	var N = 5;
	var scale = 1e-148;
	var d = new Float64Array( [ 2 * scale, 2 * scale, 2 * scale, 2 * scale, 2 * scale ] );
	var e = new Float64Array( [ -1 * scale, -1 * scale, -1 * scale, -1 * scale ] );
	var Z = new Float64Array( 1 );
	var WORK = new Float64Array( 1 );
	var info;

	info = dstev( 'N', N, d, 1, 0, e, 1, 0, Z, 1, 1, 0, WORK, 1, 0 );

	assert.equal( info, 0, 'info' );
	// Eigenvalues should be in ascending order
	for ( var i = 0; i < N - 1; i++ ) {
		assert.ok( d[ i ] <= d[ i + 1 ] + 1e-300, 'eigenvalues sorted' );
	}
});


// HELPERS //

/**
* Assert an eigenvector column matches the expected (up to sign flip).
*
* @param {Float64Array} Z - computed eigenvector matrix (column-major, N x N)
* @param {integer} N - dimension
* @param {integer} col - column index (0-based)
* @param {Array} expected - expected flat column-major eigenvector data
* @param {number} tol - tolerance
* @param {string} msg - message prefix
*/
function assertEigenvectorClose( Z, N, col, expected, tol, msg ) {
	var sign;
	var j;
	var e;
	var a;

	// Determine sign by checking first non-negligible element
	sign = 1;
	for ( j = 0; j < N; j++ ) {
		a = Z[ col * N + j ];
		e = expected[ col * N + j ];
		if ( Math.abs( e ) > 1e-10 ) {
			if ( ( a > 0 ) !== ( e > 0 ) ) {
				sign = -1;
			}
			break;
		}
	}

	for ( j = 0; j < N; j++ ) {
		a = sign * Z[ col * N + j ];
		e = expected[ col * N + j ];
		assertClose( a, e, tol, msg + '[' + j + ']' );
	}
}
