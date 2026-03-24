

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zgbtrf = require( '../../zgbtrf/lib/base.js' );
var zgbtrs = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zgbtrs.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assert.ok(
			Math.abs( actual[ i ] - expected[ i ] ) <= tol,
			msg + '[' + i + ']: expected ' + expected[ i ] + ', got ' + actual[ i ]
		);
	}
}


// TESTS //

test( 'zgbtrs: main export is a function', function t() {
	assert.strictEqual( typeof zgbtrs, 'function' );
});

test( 'zgbtrs: no-transpose, 4x4 tridiagonal (KL=1, KU=1)', function t() {
	var tc = findCase( 'notrans_tridiag' );
	// A = [4+i  -1    0     0  ]
	//     [-1    4+i  -1    0  ]
	//     [ 0   -1    4+i  -1  ]
	//     [ 0    0   -1     4+i]
	// LDAB = 2*KL+KU+1 = 4, stored column-major
	// Row 0: KU row (extra fill), Row 1: superdiag, Row 2: diag (KL+KU), Row 3: subdiag
	var AB = new Complex128Array( 4 * 4 );
	var ABv = reinterpret( AB, 0 );
	var IPIV = new Int32Array( 4 );
	var B = new Complex128Array( 4 );
	var Bv = reinterpret( B, 0 );
	var info;

	// Fill AB in column-major, LDAB=4
	// Col 0: rows [*, *, 4+i, -1] (superdiag=*, diag at row 2, subdiag at row 3)
	ABv[ 4 ] = 4; ABv[ 5 ] = 1;   // AB(2,0) = 4+i (diag)
	ABv[ 6 ] = -1; ABv[ 7 ] = 0;  // AB(3,0) = -1 (subdiag)
	// Col 1:
	ABv[ 10 ] = -1; ABv[ 11 ] = 0; // AB(1,1) = -1 (superdiag)
	ABv[ 12 ] = 4; ABv[ 13 ] = 1;  // AB(2,1) = 4+i (diag)
	ABv[ 14 ] = -1; ABv[ 15 ] = 0; // AB(3,1) = -1 (subdiag)
	// Col 2:
	ABv[ 18 ] = -1; ABv[ 19 ] = 0; // AB(1,2) = -1 (superdiag)
	ABv[ 20 ] = 4; ABv[ 21 ] = 1;  // AB(2,2) = 4+i (diag)
	ABv[ 22 ] = -1; ABv[ 23 ] = 0; // AB(3,2) = -1 (subdiag)
	// Col 3:
	ABv[ 26 ] = -1; ABv[ 27 ] = 0; // AB(1,3) = -1 (superdiag)
	ABv[ 28 ] = 4; ABv[ 29 ] = 1;  // AB(2,3) = 4+i (diag)

	// Factorize
	info = zgbtrf( 4, 4, 1, 1, AB, 1, 4, 0, IPIV, 1, 0 );
	assert.strictEqual( info, 0 );

	// Set up RHS: b = [1, 2, 3, 4]
	Bv[ 0 ] = 1; Bv[ 1 ] = 0;
	Bv[ 2 ] = 2; Bv[ 3 ] = 0;
	Bv[ 4 ] = 3; Bv[ 5 ] = 0;
	Bv[ 6 ] = 4; Bv[ 7 ] = 0;

	// Solve A*X = B
	info = zgbtrs( 'no-transpose', 4, 1, 1, 1, AB, 1, 4, 0, IPIV, 1, 0, B, 1, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( Array.from( Bv ), tc.B, 1e-12, 'B' );
});

test( 'zgbtrs: transpose, 4x4 tridiagonal', function t() {
	var tc = findCase( 'trans_tridiag' );
	var AB = new Complex128Array( 4 * 4 );
	var ABv = reinterpret( AB, 0 );
	var IPIV = new Int32Array( 4 );
	var B = new Complex128Array( 4 );
	var Bv = reinterpret( B, 0 );
	var info;

	// Same matrix setup
	ABv[ 4 ] = 4; ABv[ 5 ] = 1;
	ABv[ 6 ] = -1; ABv[ 7 ] = 0;
	ABv[ 10 ] = -1; ABv[ 11 ] = 0;
	ABv[ 12 ] = 4; ABv[ 13 ] = 1;
	ABv[ 14 ] = -1; ABv[ 15 ] = 0;
	ABv[ 18 ] = -1; ABv[ 19 ] = 0;
	ABv[ 20 ] = 4; ABv[ 21 ] = 1;
	ABv[ 22 ] = -1; ABv[ 23 ] = 0;
	ABv[ 26 ] = -1; ABv[ 27 ] = 0;
	ABv[ 28 ] = 4; ABv[ 29 ] = 1;

	info = zgbtrf( 4, 4, 1, 1, AB, 1, 4, 0, IPIV, 1, 0 );
	assert.strictEqual( info, 0 );

	Bv[ 0 ] = 1; Bv[ 1 ] = 0;
	Bv[ 2 ] = 2; Bv[ 3 ] = 0;
	Bv[ 4 ] = 3; Bv[ 5 ] = 0;
	Bv[ 6 ] = 4; Bv[ 7 ] = 0;

	info = zgbtrs( 'transpose', 4, 1, 1, 1, AB, 1, 4, 0, IPIV, 1, 0, B, 1, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( Array.from( Bv ), tc.B, 1e-12, 'B' );
});

test( 'zgbtrs: conjugate-transpose, 4x4 tridiagonal', function t() {
	var tc = findCase( 'conjtrans_tridiag' );
	var AB = new Complex128Array( 4 * 4 );
	var ABv = reinterpret( AB, 0 );
	var IPIV = new Int32Array( 4 );
	var B = new Complex128Array( 4 );
	var Bv = reinterpret( B, 0 );
	var info;

	ABv[ 4 ] = 4; ABv[ 5 ] = 1;
	ABv[ 6 ] = -1; ABv[ 7 ] = 0;
	ABv[ 10 ] = -1; ABv[ 11 ] = 0;
	ABv[ 12 ] = 4; ABv[ 13 ] = 1;
	ABv[ 14 ] = -1; ABv[ 15 ] = 0;
	ABv[ 18 ] = -1; ABv[ 19 ] = 0;
	ABv[ 20 ] = 4; ABv[ 21 ] = 1;
	ABv[ 22 ] = -1; ABv[ 23 ] = 0;
	ABv[ 26 ] = -1; ABv[ 27 ] = 0;
	ABv[ 28 ] = 4; ABv[ 29 ] = 1;

	info = zgbtrf( 4, 4, 1, 1, AB, 1, 4, 0, IPIV, 1, 0 );
	assert.strictEqual( info, 0 );

	Bv[ 0 ] = 1; Bv[ 1 ] = 0;
	Bv[ 2 ] = 2; Bv[ 3 ] = 0;
	Bv[ 4 ] = 3; Bv[ 5 ] = 0;
	Bv[ 6 ] = 4; Bv[ 7 ] = 0;

	info = zgbtrs( 'conjugate-transpose', 4, 1, 1, 1, AB, 1, 4, 0, IPIV, 1, 0, B, 1, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( Array.from( Bv ), tc.B, 1e-12, 'B' );
});

test( 'zgbtrs: multiple RHS (NRHS=2)', function t() {
	var tc = findCase( 'nrhs_2' );
	var AB = new Complex128Array( 4 * 4 );
	var ABv = reinterpret( AB, 0 );
	var IPIV = new Int32Array( 4 );
	// B is 4x2 column-major, LDB=4 (stored in complex elements)
	var B = new Complex128Array( 4 * 2 );
	var Bv = reinterpret( B, 0 );
	var info;

	ABv[ 4 ] = 4; ABv[ 5 ] = 1;
	ABv[ 6 ] = -1; ABv[ 7 ] = 0;
	ABv[ 10 ] = -1; ABv[ 11 ] = 0;
	ABv[ 12 ] = 4; ABv[ 13 ] = 1;
	ABv[ 14 ] = -1; ABv[ 15 ] = 0;
	ABv[ 18 ] = -1; ABv[ 19 ] = 0;
	ABv[ 20 ] = 4; ABv[ 21 ] = 1;
	ABv[ 22 ] = -1; ABv[ 23 ] = 0;
	ABv[ 26 ] = -1; ABv[ 27 ] = 0;
	ABv[ 28 ] = 4; ABv[ 29 ] = 1;

	info = zgbtrf( 4, 4, 1, 1, AB, 1, 4, 0, IPIV, 1, 0 );
	assert.strictEqual( info, 0 );

	// Column 0 of B: [1, 2, 3, 4]
	Bv[ 0 ] = 1; Bv[ 1 ] = 0;
	Bv[ 2 ] = 2; Bv[ 3 ] = 0;
	Bv[ 4 ] = 3; Bv[ 5 ] = 0;
	Bv[ 6 ] = 4; Bv[ 7 ] = 0;
	// Column 1 of B: [5+i, 6+2i, 7+3i, 8+4i]
	Bv[ 8 ] = 5; Bv[ 9 ] = 1;
	Bv[ 10 ] = 6; Bv[ 11 ] = 2;
	Bv[ 12 ] = 7; Bv[ 13 ] = 3;
	Bv[ 14 ] = 8; Bv[ 15 ] = 4;

	// strideB1=1, strideB2=4 (column-major with LDB=4 in complex elements)
	info = zgbtrs( 'no-transpose', 4, 1, 1, 2, AB, 1, 4, 0, IPIV, 1, 0, B, 1, 4, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( Array.from( Bv ), tc.B, 1e-12, 'B' );
});

test( 'zgbtrs: N=0 quick return', function t() {
	var tc = findCase( 'n_zero' );
	var AB = new Complex128Array( 4 );
	var IPIV = new Int32Array( 4 );
	var B = new Complex128Array( 4 );
	var info = zgbtrs( 'no-transpose', 0, 1, 1, 1, AB, 1, 4, 0, IPIV, 1, 0, B, 1, 1, 0 );
	assert.strictEqual( info, tc.info );
});

test( 'zgbtrs: NRHS=0 quick return', function t() {
	var tc = findCase( 'nrhs_zero' );
	var AB = new Complex128Array( 4 );
	var IPIV = new Int32Array( 4 );
	var B = new Complex128Array( 4 );
	var info = zgbtrs( 'no-transpose', 4, 1, 1, 0, AB, 1, 4, 0, IPIV, 1, 0, B, 1, 1, 0 );
	assert.strictEqual( info, tc.info );
});

test( 'zgbtrs: 5x5 pentadiagonal (KL=2, KU=2)', function t() {
	var tc = findCase( 'pentadiag_5x5' );
	// LDAB = 2*KL+KU+1 = 7
	var AB = new Complex128Array( 7 * 5 );
	var ABv = reinterpret( AB, 0 );
	var IPIV = new Int32Array( 5 );
	var B = new Complex128Array( 5 );
	var Bv = reinterpret( B, 0 );
	var info;

	// Diagonal at row KL+KU = 4 (0-based), stored column-major LDAB=7
	// Col 0: diag at row 4
	ABv[ 8 ] = 6; ABv[ 9 ] = 1;    // AB(4,0) = 6+i (diag)
	ABv[ 10 ] = -2; ABv[ 11 ] = 0;  // AB(5,0) = -2 (sub1)
	ABv[ 12 ] = 1; ABv[ 13 ] = 0;   // AB(6,0) = 1 (sub2)
	// Col 1:
	ABv[ 20 ] = -2; ABv[ 21 ] = 0;  // AB(3,1) = -2 (super1)
	ABv[ 22 ] = 6; ABv[ 23 ] = 1;   // AB(4,1) = 6+i
	ABv[ 24 ] = -2; ABv[ 25 ] = 0;  // AB(5,1) = -2
	ABv[ 26 ] = 1; ABv[ 27 ] = 0;   // AB(6,1) = 1
	// Col 2:
	ABv[ 32 ] = 1; ABv[ 33 ] = 0;   // AB(2,2) = 1 (super2)
	ABv[ 34 ] = -2; ABv[ 35 ] = 0;  // AB(3,2) = -2
	ABv[ 36 ] = 6; ABv[ 37 ] = 1;   // AB(4,2) = 6+i
	ABv[ 38 ] = -2; ABv[ 39 ] = 0;  // AB(5,2) = -2
	ABv[ 40 ] = 1; ABv[ 41 ] = 0;   // AB(6,2) = 1
	// Col 3:
	ABv[ 46 ] = 1; ABv[ 47 ] = 0;   // AB(2,3) = 1
	ABv[ 48 ] = -2; ABv[ 49 ] = 0;  // AB(3,3) = -2
	ABv[ 50 ] = 6; ABv[ 51 ] = 1;   // AB(4,3) = 6+i
	ABv[ 52 ] = -2; ABv[ 53 ] = 0;  // AB(5,3) = -2
	// Col 4:
	ABv[ 60 ] = 1; ABv[ 61 ] = 0;   // AB(2,4) = 1
	ABv[ 62 ] = -2; ABv[ 63 ] = 0;  // AB(3,4) = -2
	ABv[ 64 ] = 6; ABv[ 65 ] = 1;   // AB(4,4) = 6+i

	info = zgbtrf( 5, 5, 2, 2, AB, 1, 7, 0, IPIV, 1, 0 );
	assert.strictEqual( info, 0 );

	// b = [1+i, 2+2i, 3+3i, 4+4i, 5+5i]
	Bv[ 0 ] = 1; Bv[ 1 ] = 1;
	Bv[ 2 ] = 2; Bv[ 3 ] = 2;
	Bv[ 4 ] = 3; Bv[ 5 ] = 3;
	Bv[ 6 ] = 4; Bv[ 7 ] = 4;
	Bv[ 8 ] = 5; Bv[ 9 ] = 5;

	info = zgbtrs( 'no-transpose', 5, 2, 2, 1, AB, 1, 7, 0, IPIV, 1, 0, B, 1, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( Array.from( Bv ), tc.B, 1e-12, 'B' );
});

test( 'zgbtrs: KL=0 upper triangular', function t() {
	var tc = findCase( 'kl0_upper' );
	// A = [2+i  1; 0  3+i], KL=0, KU=1, LDAB = 2*0+1+1 = 2
	var AB = new Complex128Array( 2 * 2 );
	var ABv = reinterpret( AB, 0 );
	var IPIV = new Int32Array( 2 );
	var B = new Complex128Array( 2 );
	var Bv = reinterpret( B, 0 );
	var info;

	// Col 0: row 0 = *, row 1 = 2+i
	ABv[ 2 ] = 2; ABv[ 3 ] = 1;    // AB(1,0) = 2+i (diag)
	// Col 1: row 0 = 1, row 1 = 3+i
	ABv[ 4 ] = 1; ABv[ 5 ] = 0;    // AB(0,1) = 1 (superdiag)
	ABv[ 6 ] = 3; ABv[ 7 ] = 1;    // AB(1,1) = 3+i (diag)

	info = zgbtrf( 2, 2, 0, 1, AB, 1, 2, 0, IPIV, 1, 0 );
	assert.strictEqual( info, 0 );

	// b = [5+3i, 6+4i]
	Bv[ 0 ] = 5; Bv[ 1 ] = 3;
	Bv[ 2 ] = 6; Bv[ 3 ] = 4;

	info = zgbtrs( 'no-transpose', 2, 0, 1, 1, AB, 1, 2, 0, IPIV, 1, 0, B, 1, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( Array.from( Bv ), tc.B, 1e-12, 'B' );
});
