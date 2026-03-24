

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dsygst = require( './../lib/base.js' );
var dpotrf = require( '../../dpotrf/lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dsygst.jsonl' ), 'utf8' ).trim().split( '\n' );
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

function makeBUpper() {
	var B = new Float64Array([
		4.0, 0.0, 0.0,
		2.0, 5.0, 0.0,
		0.0, 1.0, 3.0
	]);
	dpotrf( 'upper', 3, B, 1, 3, 0 );
	return B;
}

function makeBLower() {
	var B = new Float64Array([
		4.0, 2.0, 0.0,
		0.0, 5.0, 1.0,
		0.0, 0.0, 3.0
	]);
	dpotrf( 'lower', 3, B, 1, 3, 0 );
	return B;
}

function makeAUpper() {
	return new Float64Array([
		4.0, 0.0, 0.0,
		2.0, 5.0, 0.0,
		1.0, 3.0, 6.0
	]);
}

function makeALower() {
	return new Float64Array([
		4.0, 2.0, 1.0,
		0.0, 5.0, 3.0,
		0.0, 0.0, 6.0
	]);
}


// TESTS //

test( 'dsygst: itype1_upper', function t() {
	var tc = findCase( 'itype1_upper' );
	var A = makeAUpper();
	var B = makeBUpper();
	var info = dsygst( 1, 'upper', 3, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( A ), tc.A, 1e-14, 'A' );
});

test( 'dsygst: itype1_lower', function t() {
	var tc = findCase( 'itype1_lower' );
	var A = makeALower();
	var B = makeBLower();
	var info = dsygst( 1, 'lower', 3, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( A ), tc.A, 1e-14, 'A' );
});

test( 'dsygst: itype2_upper', function t() {
	var tc = findCase( 'itype2_upper' );
	var A = makeAUpper();
	var B = makeBUpper();
	var info = dsygst( 2, 'upper', 3, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( A ), tc.A, 1e-14, 'A' );
});

test( 'dsygst: itype2_lower', function t() {
	var tc = findCase( 'itype2_lower' );
	var A = makeALower();
	var B = makeBLower();
	var info = dsygst( 2, 'lower', 3, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( A ), tc.A, 1e-14, 'A' );
});

test( 'dsygst: itype3_lower', function t() {
	var tc = findCase( 'itype3_lower' );
	var A = makeALower();
	var B = makeBLower();
	var info = dsygst( 3, 'lower', 3, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( A ), tc.A, 1e-14, 'A' );
});

test( 'dsygst: n_zero', function t() {
	var tc = findCase( 'n_zero' );
	var A = new Float64Array( 1 );
	var B = new Float64Array( 1 );
	var info = dsygst( 1, 'upper', 0, A, 1, 1, 0, B, 1, 1, 0 );
	assert.equal( info, tc.info );
});

test( 'dsygst: n_one', function t() {
	var tc = findCase( 'n_one' );
	var A = new Float64Array([ 9.0 ]);
	var B = new Float64Array([ 3.0 ]);
	var info = dsygst( 1, 'upper', 1, A, 1, 1, 0, B, 1, 1, 0 );
	assert.equal( info, tc.info );
	assertClose( A[ 0 ], tc.A11, 1e-14, 'A11' );
});

// Helper to build N=70 diagonally dominant SPD matrix B (column-major flat)
function makeBigB( uplo ) {
	var N = 70;
	var B = new Float64Array( N * N );
	var i;
	var j;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < N; i++ ) {
			if ( i === j ) {
				B[ j * N + i ] = N + 1.0;
			} else if ( Math.abs( i - j ) === 1 ) {
				B[ j * N + i ] = 0.5;
			}
		}
	}
	dpotrf( uplo, N, B, 1, N, 0 );
	return B;
}

// Helper to build N=70 symmetric A in upper storage (column-major flat)
function makeBigAUpper() {
	var N = 70;
	var A = new Float64Array( N * N );
	var i;
	var j;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i <= j; i++ ) {
			if ( i === j ) {
				A[ j * N + i ] = 2 * N + ( i + 1 );
			} else {
				A[ j * N + i ] = 0.1 * ( ( i + 1 ) + ( j + 1 ) );
			}
		}
	}
	return A;
}

// Helper to build N=70 symmetric A in lower storage (column-major flat)
function makeBigALower() {
	var N = 70;
	var A = new Float64Array( N * N );
	var i;
	var j;
	for ( j = 0; j < N; j++ ) {
		for ( i = j; i < N; i++ ) {
			if ( i === j ) {
				A[ j * N + i ] = 2 * N + ( i + 1 );
			} else {
				A[ j * N + i ] = 0.1 * ( ( i + 1 ) + ( j + 1 ) );
			}
		}
	}
	return A;
}

test( 'dsygst: blocked itype1 upper N=70', function t() {
	var tc = findCase( 'blocked_itype1_upper_70' );
	var N = 70;
	var B = makeBigB( 'upper' );
	var A = makeBigAUpper();
	var info = dsygst( 1, 'upper', N, A, 1, N, 0, B, 1, N, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( A ), tc.A, 1e-10, 'A' );
});

test( 'dsygst: blocked itype1 lower N=70', function t() {
	var tc = findCase( 'blocked_itype1_lower_70' );
	var N = 70;
	var B = makeBigB( 'lower' );
	var A = makeBigALower();
	var info = dsygst( 1, 'lower', N, A, 1, N, 0, B, 1, N, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( A ), tc.A, 1e-10, 'A' );
});

test( 'dsygst: blocked itype2 upper N=70', function t() {
	var tc = findCase( 'blocked_itype2_upper_70' );
	var N = 70;
	var B = makeBigB( 'upper' );
	var A = makeBigAUpper();
	var info = dsygst( 2, 'upper', N, A, 1, N, 0, B, 1, N, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( A ), tc.A, 1e-10, 'A' );
});

test( 'dsygst: blocked itype2 lower N=70', function t() {
	var tc = findCase( 'blocked_itype2_lower_70' );
	var N = 70;
	var B = makeBigB( 'lower' );
	var A = makeBigALower();
	var info = dsygst( 2, 'lower', N, A, 1, N, 0, B, 1, N, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( A ), tc.A, 1e-10, 'A' );
});

test( 'dsygst: blocked itype3 upper N=70', function t() {
	var tc = findCase( 'blocked_itype3_upper_70' );
	var N = 70;
	var B = makeBigB( 'upper' );
	var A = makeBigAUpper();
	var info = dsygst( 3, 'upper', N, A, 1, N, 0, B, 1, N, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( A ), tc.A, 1e-10, 'A' );
});

test( 'dsygst: blocked itype3 lower N=70', function t() {
	var tc = findCase( 'blocked_itype3_lower_70' );
	var N = 70;
	var B = makeBigB( 'lower' );
	var A = makeBigALower();
	var info = dsygst( 3, 'lower', N, A, 1, N, 0, B, 1, N, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( A ), tc.A, 1e-10, 'A' );
});
