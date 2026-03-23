'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dsytrf = require( './../lib/base.js' );
var dsytrs = require( '../../dsytrs/lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dsytrf.jsonl' ), 'utf8' ).trim().split( '\n' );
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

function convertIPIV( fipiv ) {
	var out = [];
	var i;
	for ( i = 0; i < fipiv.length; i++ ) {
		if ( fipiv[ i ] > 0 ) {
			out.push( fipiv[ i ] - 1 );
		} else {
			out.push( fipiv[ i ] );
		}
	}
	return out;
}


// TESTS //

test( 'dsytrf: 4x4_lower', function t() {
	var ipiv = new Int32Array( 4 );
	var tc = findCase( '4x4_lower' );
	var A = new Float64Array([
		4, 2, 1, 0,
		0, 5, 2, 1,
		0, 0, 6, 3,
		0, 0, 0, 8
	]);
	var info = dsytrf( 'lower', 4, A, 1, 4, 0, ipiv, 1, 0 );
	assertArrayClose( A, tc.a, 1e-14, 'a' );
	assert.equal( info, tc.info, 'info' );
	assert.deepEqual( Array.from( ipiv ), convertIPIV( tc.ipiv ), 'ipiv' );
});

test( 'dsytrf: 4x4_upper', function t() {
	var ipiv = new Int32Array( 4 );
	var tc = findCase( '4x4_upper' );
	var A = new Float64Array( 16 );
	A[ 0 ] = 4;
	A[ 4 ] = 2; A[ 5 ] = 5;
	A[ 8 ] = 1; A[ 9 ] = 2; A[ 10 ] = 6;
	A[ 12 ] = 0; A[ 13 ] = 1; A[ 14 ] = 3; A[ 15 ] = 8;
	var info = dsytrf( 'upper', 4, A, 1, 4, 0, ipiv, 1, 0 );
	assertArrayClose( A, tc.a, 1e-14, 'a' );
	assert.equal( info, tc.info, 'info' );
	assert.deepEqual( Array.from( ipiv ), convertIPIV( tc.ipiv ), 'ipiv' );
});

test( 'dsytrf: 4x4_indef_lower', function t() {
	var ipiv = new Int32Array( 4 );
	var tc = findCase( '4x4_indef_lower' );
	var A = new Float64Array([
		0, 1, 2, 3,
		0, 0, 4, 5,
		0, 0, 0, 6,
		0, 0, 0, 0
	]);
	var info = dsytrf( 'lower', 4, A, 1, 4, 0, ipiv, 1, 0 );
	assertArrayClose( A, tc.a, 1e-14, 'a' );
	assert.equal( info, tc.info, 'info' );
	assert.deepEqual( Array.from( ipiv ), convertIPIV( tc.ipiv ), 'ipiv' );
});

test( 'dsytrf: 4x4_indef_upper', function t() {
	var ipiv = new Int32Array( 4 );
	var tc = findCase( '4x4_indef_upper' );
	var A = new Float64Array( 16 );
	A[ 0 ] = 0;
	A[ 4 ] = 1; A[ 5 ] = 0;
	A[ 8 ] = 2; A[ 9 ] = 4; A[ 10 ] = 0;
	A[ 12 ] = 3; A[ 13 ] = 5; A[ 14 ] = 6; A[ 15 ] = 0;
	var info = dsytrf( 'upper', 4, A, 1, 4, 0, ipiv, 1, 0 );
	assertArrayClose( A, tc.a, 1e-14, 'a' );
	assert.equal( info, tc.info, 'info' );
	assert.deepEqual( Array.from( ipiv ), convertIPIV( tc.ipiv ), 'ipiv' );
});

test( 'dsytrf: n_zero', function t() {
	var ipiv = new Int32Array( 1 );
	var A = new Float64Array( 1 );
	var info = dsytrf( 'lower', 0, A, 1, 1, 0, ipiv, 1, 0 );
	assert.equal( info, 0, 'info' );
});

test( 'dsytrf: n_one', function t() {
	var ipiv = new Int32Array( 1 );
	var tc = findCase( 'n_one' );
	var A = new Float64Array([ 7.0 ]);
	var info = dsytrf( 'lower', 1, A, 1, 1, 0, ipiv, 1, 0 );
	assertArrayClose( A, tc.a, 1e-14, 'a' );
	assert.equal( info, tc.info, 'info' );
	assert.deepEqual( Array.from( ipiv ), convertIPIV( tc.ipiv ), 'ipiv' );
});

test( 'dsytrf: singular', function t() {
	var ipiv = new Int32Array( 2 );
	var tc = findCase( 'singular' );
	var A = new Float64Array([ 0, 0, 0, 0 ]);
	var info = dsytrf( 'lower', 2, A, 1, 2, 0, ipiv, 1, 0 );
	assertArrayClose( A, tc.a, 1e-14, 'a' );
	assert.equal( info, tc.info, 'info' );
	assert.deepEqual( Array.from( ipiv ), convertIPIV( tc.ipiv ), 'ipiv' );
});

test( 'dsytrf: 5x5_lower', function t() {
	var ipiv = new Int32Array( 5 );
	var tc = findCase( '5x5_lower' );
	var A = new Float64Array([
		1, -2, 0, 3, 1,
		0, 0, 4, -1, 2,
		0, 0, -3, 2, 0,
		0, 0, 0, 1, -2,
		0, 0, 0, 0, 4
	]);
	var info = dsytrf( 'lower', 5, A, 1, 5, 0, ipiv, 1, 0 );
	assertArrayClose( A, tc.a, 1e-14, 'a' );
	assert.equal( info, tc.info, 'info' );
	assert.deepEqual( Array.from( ipiv ), convertIPIV( tc.ipiv ), 'ipiv' );
});

test( 'dsytrf: 40x40 blocked lower (exercise dlasyf)', function t() {
	// Create a 40x40 diagonally dominant symmetric matrix to exercise blocked path (NB=32)
	var N = 40;
	var A = new Float64Array( N * N );
	var Asave = new Float64Array( N * N );
	var b = new Float64Array( N );
	var x = new Float64Array( N );
	var ipiv = new Int32Array( N );
	var sum;
	var i;
	var j;

	// Build symmetric diagonally dominant matrix (lower triangle stored)
	for ( j = 0; j < N; j++ ) {
		for ( i = j; i < N; i++ ) {
			if ( i === j ) {
				A[ i + j * N ] = 3.0 * N;
			} else {
				A[ i + j * N ] = ( ( i + j ) % 7 ) - 3.0; // values in [-3, 3]
			}
		}
	}
	Asave.set( A );

	// x = [1, 2, 3, ..., N], compute b = A*x using full symmetric matrix
	for ( i = 0; i < N; i++ ) {
		x[ i ] = i + 1.0;
	}
	for ( i = 0; i < N; i++ ) {
		sum = 0.0;
		for ( j = 0; j < N; j++ ) {
			if ( j <= i ) {
				sum += Asave[ i + j * N ] * x[ j ];
			} else {
				sum += Asave[ j + i * N ] * x[ j ];
			}
		}
		b[ i ] = sum;
	}

	var info = dsytrf( 'lower', N, A, 1, N, 0, ipiv, 1, 0 );
	assert.equal( info, 0, 'factor info' );

	var info2 = dsytrs( 'lower', N, 1, A, 1, N, 0, ipiv, 1, 0, b, 1, N, 0 );
	assert.equal( info2, 0, 'solve info' );

	// Verify b ≈ x
	for ( i = 0; i < N; i++ ) {
		assertClose( b[ i ], x[ i ], 1e-10, 'x[' + i + ']' );
	}
});

test( 'dsytrf: 40x40 blocked upper (exercise dlasyf)', function t() {
	var N = 40;
	var A = new Float64Array( N * N );
	var Asave = new Float64Array( N * N );
	var b = new Float64Array( N );
	var x = new Float64Array( N );
	var ipiv = new Int32Array( N );
	var sum;
	var i;
	var j;

	// Build symmetric matrix (upper triangle stored)
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i <= j; i++ ) {
			if ( i === j ) {
				A[ i + j * N ] = 3.0 * N;
			} else {
				A[ i + j * N ] = ( ( i + j ) % 7 ) - 3.0;
			}
		}
	}
	Asave.set( A );

	for ( i = 0; i < N; i++ ) {
		x[ i ] = i + 1.0;
	}
	for ( i = 0; i < N; i++ ) {
		sum = 0.0;
		for ( j = 0; j < N; j++ ) {
			if ( i <= j ) {
				sum += Asave[ i + j * N ] * x[ j ];
			} else {
				sum += Asave[ j + i * N ] * x[ j ];
			}
		}
		b[ i ] = sum;
	}

	var info = dsytrf( 'upper', N, A, 1, N, 0, ipiv, 1, 0 );
	assert.equal( info, 0, 'factor info' );

	var info2 = dsytrs( 'upper', N, 1, A, 1, N, 0, ipiv, 1, 0, b, 1, N, 0 );
	assert.equal( info2, 0, 'solve info' );

	for ( i = 0; i < N; i++ ) {
		assertClose( b[ i ], x[ i ], 1e-10, 'x[' + i + ']' );
	}
});
