'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var dormqr = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dormqr.jsonl' ), 'utf8' ).trim().split( '\n' );
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
* Return the QR factors (A and TAU) for the 4x3 test matrix from the fixture.
* These were produced by Fortran's dgeqrf with LDA=6.
*/
function qr4x3() {
	var tc = findCase( 'qr_factors_small' );
	return {
		A: new Float64Array( tc.a ),
		TAU: new Float64Array( tc.tau )
	};
}

/**
* Return the QR factors for the 40x35 test matrix from the fixture.
* These were produced by Fortran's dgeqrf with LDA=40.
*/
function qr40x35() {
	var tc = findCase( 'qr_factors_large' );
	return {
		A: new Float64Array( tc.a ),
		TAU: new Float64Array( tc.tau )
	};
}

/**
* Create an identity matrix in column-major layout.
*/
function eye( n, lda ) {
	var C = new Float64Array( lda * n );
	var i;
	for ( i = 0; i < n; i++ ) {
		C[ i * lda + i ] = 1.0;
	}
	return C;
}


// TESTS //

test( 'dormqr: left_notrans (Q*I)', function t() {
	var tc = findCase( 'left_notrans' );
	var qr = qr4x3();
	var C = eye( 4, 6 );
	var WORK = new Float64Array( 1000 );
	var info = dormqr( 'left', 'no-transpose', 4, 4, 3,
		qr.A, 1, 6, 0,
		qr.TAU, 1, 0,
		C, 1, 6, 0,
		WORK, 1, 0, 1000
	);
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( C ), tc.c, 1e-14, 'c' );
});

test( 'dormqr: left_trans (Q^T*I)', function t() {
	var tc = findCase( 'left_trans' );
	var qr = qr4x3();
	var C = eye( 4, 6 );
	var WORK = new Float64Array( 1000 );
	var info = dormqr( 'left', 'transpose', 4, 4, 3,
		qr.A, 1, 6, 0,
		qr.TAU, 1, 0,
		C, 1, 6, 0,
		WORK, 1, 0, 1000
	);
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( C ), tc.c, 1e-14, 'c' );
});

test( 'dormqr: right_notrans (I*Q)', function t() {
	var tc = findCase( 'right_notrans' );
	var qr = qr4x3();
	var C = eye( 4, 6 );
	var WORK = new Float64Array( 1000 );
	var info = dormqr( 'right', 'no-transpose', 4, 4, 3,
		qr.A, 1, 6, 0,
		qr.TAU, 1, 0,
		C, 1, 6, 0,
		WORK, 1, 0, 1000
	);
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( C ), tc.c, 1e-14, 'c' );
});

test( 'dormqr: right_trans (I*Q^T)', function t() {
	var tc = findCase( 'right_trans' );
	var qr = qr4x3();
	var C = eye( 4, 6 );
	var WORK = new Float64Array( 1000 );
	var info = dormqr( 'right', 'transpose', 4, 4, 3,
		qr.A, 1, 6, 0,
		qr.TAU, 1, 0,
		C, 1, 6, 0,
		WORK, 1, 0, 1000
	);
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( C ), tc.c, 1e-14, 'c' );
});

test( 'dormqr: m_zero (quick return)', function t() {
	var tc = findCase( 'm_zero' );
	var qr = qr4x3();
	var C = new Float64Array( 1 );
	var WORK = new Float64Array( 1 );
	var info = dormqr( 'left', 'no-transpose', 0, 4, 0,
		qr.A, 1, 6, 0,
		qr.TAU, 1, 0,
		C, 1, 1, 0,
		WORK, 1, 0, 1
	);
	assert.equal( info, tc.info );
});

test( 'dormqr: n_zero (quick return)', function t() {
	var tc = findCase( 'n_zero' );
	var qr = qr4x3();
	var C = new Float64Array( 1 );
	var WORK = new Float64Array( 1 );
	var info = dormqr( 'left', 'no-transpose', 4, 0, 0,
		qr.A, 1, 6, 0,
		qr.TAU, 1, 0,
		C, 1, 6, 0,
		WORK, 1, 0, 1
	);
	assert.equal( info, tc.info );
});

test( 'dormqr: k_zero (quick return)', function t() {
	var tc = findCase( 'k_zero' );
	var qr = qr4x3();
	var C = eye( 4, 6 );
	var Cexpected = eye( 4, 6 );
	var WORK = new Float64Array( 1 );
	var info = dormqr( 'left', 'no-transpose', 4, 4, 0,
		qr.A, 1, 6, 0,
		qr.TAU, 1, 0,
		C, 1, 6, 0,
		WORK, 1, 0, 1
	);
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( C ), Array.from( Cexpected ), 1e-14, 'c unchanged' );
});

test( 'dormqr: left_notrans_rect (Q * non-identity 4x2)', function t() {
	var tc = findCase( 'left_notrans_rect' );
	var qr = qr4x3();
	// C is 4x2 in LDA=6 container, column-major
	var C = new Float64Array( 6 * 6 );
	C[ 0 ] = 1.0; C[ 1 ] = 3.0; C[ 2 ] = -1.0; C[ 3 ] = 2.0;
	C[ 6 ] = 2.0; C[ 7 ] = 0.0; C[ 8 ] = 4.0; C[ 9 ] = -1.0;
	var WORK = new Float64Array( 1000 );
	var info = dormqr( 'left', 'no-transpose', 4, 2, 3,
		qr.A, 1, 6, 0,
		qr.TAU, 1, 0,
		C, 1, 6, 0,
		WORK, 1, 0, 1000
	);
	assert.equal( info, tc.info );
	// Compare first 12 elements (LDA=6, 2 columns)
	var result = Array.from( C ).slice( 0, 12 );
	assertArrayClose( result, tc.c, 1e-14, 'c' );
});

test( 'dormqr: left_trans_rect (Q^T * non-identity 4x2)', function t() {
	var tc = findCase( 'left_trans_rect' );
	var qr = qr4x3();
	var C = new Float64Array( 6 * 6 );
	C[ 0 ] = 1.0; C[ 1 ] = 3.0; C[ 2 ] = -1.0; C[ 3 ] = 2.0;
	C[ 6 ] = 2.0; C[ 7 ] = 0.0; C[ 8 ] = 4.0; C[ 9 ] = -1.0;
	var WORK = new Float64Array( 1000 );
	var info = dormqr( 'left', 'transpose', 4, 2, 3,
		qr.A, 1, 6, 0,
		qr.TAU, 1, 0,
		C, 1, 6, 0,
		WORK, 1, 0, 1000
	);
	assert.equal( info, tc.info );
	var result = Array.from( C ).slice( 0, 12 );
	assertArrayClose( result, tc.c, 1e-14, 'c' );
});

test( 'dormqr: right_notrans_rect (3x4 * Q)', function t() {
	var tc = findCase( 'right_notrans_rect' );
	var qr = qr4x3();
	// C is 3x4, column-major, LDC=3
	var C = new Float64Array( 3 * 6 );
	// Column 0
	C[ 0 ] = 1.0; C[ 1 ] = 0.0; C[ 2 ] = 2.0;
	// Column 1
	C[ 3 ] = 2.0; C[ 4 ] = 1.0; C[ 5 ] = -1.0;
	// Column 2
	C[ 6 ] = -1.0; C[ 7 ] = 3.0; C[ 8 ] = 0.0;
	// Column 3
	C[ 9 ] = 4.0; C[ 10 ] = -2.0; C[ 11 ] = 1.0;
	var WORK = new Float64Array( 1000 );
	var info = dormqr( 'right', 'no-transpose', 3, 4, 3,
		qr.A, 1, 6, 0,
		qr.TAU, 1, 0,
		C, 1, 3, 0,
		WORK, 1, 0, 1000
	);
	assert.equal( info, tc.info );
	var result = Array.from( C ).slice( 0, 12 );
	assertArrayClose( result, tc.c, 1e-14, 'c' );
});

test( 'dormqr: right_trans_rect (3x4 * Q^T)', function t() {
	var tc = findCase( 'right_trans_rect' );
	var qr = qr4x3();
	var C = new Float64Array( 3 * 6 );
	C[ 0 ] = 1.0; C[ 1 ] = 0.0; C[ 2 ] = 2.0;
	C[ 3 ] = 2.0; C[ 4 ] = 1.0; C[ 5 ] = -1.0;
	C[ 6 ] = -1.0; C[ 7 ] = 3.0; C[ 8 ] = 0.0;
	C[ 9 ] = 4.0; C[ 10 ] = -2.0; C[ 11 ] = 1.0;
	var WORK = new Float64Array( 1000 );
	var info = dormqr( 'right', 'transpose', 3, 4, 3,
		qr.A, 1, 6, 0,
		qr.TAU, 1, 0,
		C, 1, 3, 0,
		WORK, 1, 0, 1000
	);
	assert.equal( info, tc.info );
	var result = Array.from( C ).slice( 0, 12 );
	assertArrayClose( result, tc.c, 1e-14, 'c' );
});

test( 'dormqr: left_notrans_blocked (40x40, K=35)', function t() {
	var tc = findCase( 'left_notrans_blocked' );
	var qr = qr40x35();
	var C = eye( 40, 40 );
	var WORK = new Float64Array( 10000 );
	var info = dormqr( 'left', 'no-transpose', 40, 40, 35,
		qr.A, 1, 40, 0,
		qr.TAU, 1, 0,
		C, 1, 40, 0,
		WORK, 1, 0, 10000
	);
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( C ), tc.c, 1e-12, 'c' );
});

test( 'dormqr: left_trans_blocked (40x40, K=35)', function t() {
	var tc = findCase( 'left_trans_blocked' );
	var qr = qr40x35();
	var C = eye( 40, 40 );
	var WORK = new Float64Array( 10000 );
	var info = dormqr( 'left', 'transpose', 40, 40, 35,
		qr.A, 1, 40, 0,
		qr.TAU, 1, 0,
		C, 1, 40, 0,
		WORK, 1, 0, 10000
	);
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( C ), tc.c, 1e-12, 'c' );
});

test( 'dormqr: right_notrans_blocked (40x40, K=35)', function t() {
	var tc = findCase( 'right_notrans_blocked' );
	var qr = qr40x35();
	var C = eye( 40, 40 );
	var WORK = new Float64Array( 10000 );
	var info = dormqr( 'right', 'no-transpose', 40, 40, 35,
		qr.A, 1, 40, 0,
		qr.TAU, 1, 0,
		C, 1, 40, 0,
		WORK, 1, 0, 10000
	);
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( C ), tc.c, 1e-12, 'c' );
});

test( 'dormqr: right_trans_blocked (40x40, K=35)', function t() {
	var tc = findCase( 'right_trans_blocked' );
	var qr = qr40x35();
	var C = eye( 40, 40 );
	var WORK = new Float64Array( 10000 );
	var info = dormqr( 'right', 'transpose', 40, 40, 35,
		qr.A, 1, 40, 0,
		qr.TAU, 1, 0,
		C, 1, 40, 0,
		WORK, 1, 0, 10000
	);
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( C ), tc.c, 1e-12, 'c' );
});

test( 'dormqr: Q * Q^T = I (orthogonality check, unblocked)', function t() {
	var qr = qr4x3();
	var WORK = new Float64Array( 1000 );

	// Compute Q by applying to identity from left
	var Q = eye( 4, 4 );
	dormqr( 'left', 'no-transpose', 4, 4, 3,
		qr.A, 1, 6, 0,
		qr.TAU, 1, 0,
		Q, 1, 4, 0,
		WORK, 1, 0, 1000
	);

	// Now apply Q^T from the left to Q: result should be I
	var qr2 = qr4x3();
	dormqr( 'left', 'transpose', 4, 4, 3,
		qr2.A, 1, 6, 0,
		qr2.TAU, 1, 0,
		Q, 1, 4, 0,
		WORK, 1, 0, 1000
	);

	var I4 = eye( 4, 4 );
	assertArrayClose( Array.from( Q ), Array.from( I4 ), 1e-14, 'Q*Q^T=I' );
});

test( 'dormqr: Q * Q^T = I (orthogonality check, blocked)', function t() {
	var qr = qr40x35();
	var WORK = new Float64Array( 10000 );

	// Compute Q by applying to identity from left
	var Q = eye( 40, 40 );
	dormqr( 'left', 'no-transpose', 40, 40, 35,
		qr.A, 1, 40, 0,
		qr.TAU, 1, 0,
		Q, 1, 40, 0,
		WORK, 1, 0, 10000
	);

	// Apply Q^T from the left: result should be I
	var qr2 = qr40x35();
	dormqr( 'left', 'transpose', 40, 40, 35,
		qr2.A, 1, 40, 0,
		qr2.TAU, 1, 0,
		Q, 1, 40, 0,
		WORK, 1, 0, 10000
	);

	var I40 = eye( 40, 40 );
	assertArrayClose( Array.from( Q ), Array.from( I40 ), 1e-12, 'Q*Q^T=I blocked' );
});
