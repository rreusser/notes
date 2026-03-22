'use strict';

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dlarft = require( './../lib/base.js' );

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlarft.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var i, relErr;
	for ( i = 0; i < expected.length; i++ ) {
		relErr = Math.abs( actual[ i ] - expected[ i ] ) / Math.max( Math.abs( expected[ i ] ), 1.0 );
		if ( relErr > tol ) {
			throw new Error( msg + '[' + i + ']: expected ' + expected[ i ] + ', got ' + actual[ i ] );
		}
	}
}

// Fortran print_matrix extracts MxN submatrix from LDA-strided storage.
// In JS we use exact-size storage (LDT=K, strideT1=1, strideT2=K).

test( 'dlarft: fwd col 5x3', function t() {
	var tc = findCase( 'fwd_col_5x3' );
	var V = new Float64Array( 6 * 3 );
	V[ 0 + 0 * 6 ] = 1;
	V[ 1 + 0 * 6 ] = 0.5; V[ 1 + 1 * 6 ] = 1;
	V[ 2 + 0 * 6 ] = 0.25; V[ 2 + 1 * 6 ] = 0.5; V[ 2 + 2 * 6 ] = 1;
	V[ 3 + 0 * 6 ] = 0.125; V[ 3 + 1 * 6 ] = 0.25; V[ 3 + 2 * 6 ] = 0.5;
	V[ 4 + 0 * 6 ] = 0.0625; V[ 4 + 1 * 6 ] = 0.125; V[ 4 + 2 * 6 ] = 0.25;
	var TAU = new Float64Array( [ 1.2, 1.5, 1.1 ] );
	var T = new Float64Array( 3 * 3 );
	dlarft( 'F', 'C', 5, 3, V, 1, 6, 0, TAU, 1, 0, T, 1, 3, 0 );
	assertArrayClose( T, tc.T, 1e-14, 'T' );
});

test( 'dlarft: fwd col 3x2', function t() {
	var tc = findCase( 'fwd_col_3x2' );
	var V = new Float64Array( 6 * 2 );
	V[ 0 + 0 * 6 ] = 1;
	V[ 1 + 0 * 6 ] = 2; V[ 1 + 1 * 6 ] = 1;
	V[ 2 + 0 * 6 ] = 3; V[ 2 + 1 * 6 ] = 4;
	var TAU = new Float64Array( [ 0.8, 1.2 ] );
	// Use LDT=2 (K=2), so strideT1=1, strideT2=2
	var T = new Float64Array( 2 * 2 );
	dlarft( 'F', 'C', 3, 2, V, 1, 6, 0, TAU, 1, 0, T, 1, 2, 0 );
	assertArrayClose( T, tc.T, 1e-14, 'T' );
});

test( 'dlarft: bwd col 5x2', function t() {
	var tc = findCase( 'bwd_col_5x2' );
	var V = new Float64Array( 6 * 2 );
	V[ 0 + 0 * 6 ] = 0.5; V[ 0 + 1 * 6 ] = 0.25;
	V[ 1 + 0 * 6 ] = 0.25; V[ 1 + 1 * 6 ] = 0.125;
	V[ 2 + 0 * 6 ] = 0.125; V[ 2 + 1 * 6 ] = 0.0625;
	V[ 3 + 0 * 6 ] = 1.0;
	V[ 4 + 1 * 6 ] = 1.0;
	var TAU = new Float64Array( [ 1.5, 0.9 ] );
	var T = new Float64Array( 2 * 2 );
	dlarft( 'B', 'C', 5, 2, V, 1, 6, 0, TAU, 1, 0, T, 1, 2, 0 );
	assertArrayClose( T, tc.T, 1e-14, 'T' );
});

test( 'dlarft: fwd col tau zero', function t() {
	var tc = findCase( 'fwd_col_tau_zero' );
	var V = new Float64Array( 6 * 2 );
	V[ 0 + 0 * 6 ] = 1;
	V[ 1 + 0 * 6 ] = 0.5; V[ 1 + 1 * 6 ] = 1;
	V[ 2 + 0 * 6 ] = 0.25; V[ 2 + 1 * 6 ] = 0.5;
	var TAU = new Float64Array( [ 1.2, 0.0 ] );
	var T = new Float64Array( 2 * 2 );
	dlarft( 'F', 'C', 3, 2, V, 1, 6, 0, TAU, 1, 0, T, 1, 2, 0 );
	assertArrayClose( T, tc.T, 1e-14, 'T' );
});

test( 'dlarft: fwd row 5x3', function t() {
	var tc = findCase( 'fwd_row_5x3' );
	// V is K x N = 3 x 5 in Fortran, stored column-major with LDV=6
	// In JS: strideV1=1, strideV2=6 (same layout)
	var V = new Float64Array( 6 * 6 );
	V[ 0 + 0 * 6 ] = 1; V[ 0 + 1 * 6 ] = 0.5; V[ 0 + 2 * 6 ] = 0.25; V[ 0 + 3 * 6 ] = 0.125; V[ 0 + 4 * 6 ] = 0.0625;
	V[ 1 + 1 * 6 ] = 1; V[ 1 + 2 * 6 ] = 0.5; V[ 1 + 3 * 6 ] = 0.25; V[ 1 + 4 * 6 ] = 0.125;
	V[ 2 + 2 * 6 ] = 1; V[ 2 + 3 * 6 ] = 0.5; V[ 2 + 4 * 6 ] = 0.25;
	var TAU = new Float64Array( [ 1.2, 1.5, 1.1 ] );
	var T = new Float64Array( 3 * 3 );
	dlarft( 'F', 'R', 5, 3, V, 1, 6, 0, TAU, 1, 0, T, 1, 3, 0 );
	assertArrayClose( T, tc.T, 1e-14, 'T' );
});

test( 'dlarft: fwd row 3x2', function t() {
	var tc = findCase( 'fwd_row_3x2' );
	// V is K x N = 2 x 3 in Fortran, stored column-major with LDV=6
	var V = new Float64Array( 6 * 6 );
	V[ 0 + 0 * 6 ] = 1; V[ 0 + 1 * 6 ] = 2; V[ 0 + 2 * 6 ] = 3;
	V[ 1 + 1 * 6 ] = 1; V[ 1 + 2 * 6 ] = 4;
	var TAU = new Float64Array( [ 0.8, 1.2 ] );
	var T = new Float64Array( 2 * 2 );
	dlarft( 'F', 'R', 3, 2, V, 1, 6, 0, TAU, 1, 0, T, 1, 2, 0 );
	assertArrayClose( T, tc.T, 1e-14, 'T' );
});

test( 'dlarft: bwd row 5x2', function t() {
	var tc = findCase( 'bwd_row_5x2' );
	// V is K x N = 2 x 5 in Fortran, stored column-major with LDV=6
	// Unit triangular at right (columns N-K..N-1 = 3..4)
	var V = new Float64Array( 6 * 6 );
	V[ 0 + 0 * 6 ] = 0.5; V[ 0 + 1 * 6 ] = 0.25; V[ 0 + 2 * 6 ] = 0.125; V[ 0 + 3 * 6 ] = 1.0; V[ 0 + 4 * 6 ] = 0.0;
	V[ 1 + 0 * 6 ] = 0.25; V[ 1 + 1 * 6 ] = 0.125; V[ 1 + 2 * 6 ] = 0.0625; V[ 1 + 3 * 6 ] = 0.0; V[ 1 + 4 * 6 ] = 1.0;
	var TAU = new Float64Array( [ 1.5, 0.9 ] );
	var T = new Float64Array( 2 * 2 );
	dlarft( 'B', 'R', 5, 2, V, 1, 6, 0, TAU, 1, 0, T, 1, 2, 0 );
	assertArrayClose( T, tc.T, 1e-14, 'T' );
});

test( 'dlarft: bwd col tau zero', function t() {
	var tc = findCase( 'bwd_col_tau_zero' );
	// Same V as bwd_col_5x2 but tau(1)=0
	var V = new Float64Array( 6 * 2 );
	V[ 0 + 0 * 6 ] = 0.5; V[ 0 + 1 * 6 ] = 0.25;
	V[ 1 + 0 * 6 ] = 0.25; V[ 1 + 1 * 6 ] = 0.125;
	V[ 2 + 0 * 6 ] = 0.125; V[ 2 + 1 * 6 ] = 0.0625;
	V[ 3 + 0 * 6 ] = 1.0;
	V[ 4 + 1 * 6 ] = 1.0;
	var TAU = new Float64Array( [ 0.0, 0.9 ] );
	var T = new Float64Array( 2 * 2 );
	dlarft( 'B', 'C', 5, 2, V, 1, 6, 0, TAU, 1, 0, T, 1, 2, 0 );
	assertArrayClose( T, tc.T, 1e-14, 'T' );
});

test( 'dlarft: N=0 quick return', function t() {
	var T = new Float64Array( [ 99 ] );
	var V = new Float64Array( 1 );
	var TAU = new Float64Array( [ 1.0 ] );
	dlarft( 'F', 'C', 0, 1, V, 1, 1, 0, TAU, 1, 0, T, 1, 1, 0 );
	if ( T[ 0 ] !== 99 ) {
		throw new Error( 'T changed on N=0' );
	}
});

test( 'dlarft: fwd col with trailing zeros in V (exercises lastv assignment, lines 62-63)', function t() {
	// 6x2 V matrix, col-wise, where column 0 has trailing zeros in rows 4,5
	// This means the scan at lines 58-63 will find zeros and execute lastv=jj
	var V = new Float64Array( 8 * 2 );
	// Column 0: [1, 0.5, 0.25, 0.125, 0, 0, 0, 0] (trailing zeros at rows 4-7)
	V[ 0 + 0 * 8 ] = 1;
	V[ 1 + 0 * 8 ] = 0.5;
	V[ 2 + 0 * 8 ] = 0.25;
	V[ 3 + 0 * 8 ] = 0.125;
	V[ 4 + 0 * 8 ] = 0.0;
	V[ 5 + 0 * 8 ] = 0.0;
	// Column 1: [0, 1, 0.5, 0.25, 0.125, 0.0625, 0, 0]
	V[ 1 + 1 * 8 ] = 1;
	V[ 2 + 1 * 8 ] = 0.5;
	V[ 3 + 1 * 8 ] = 0.25;
	V[ 4 + 1 * 8 ] = 0.125;
	V[ 5 + 1 * 8 ] = 0.0625;
	var TAU = new Float64Array( [ 1.2, 1.5 ] );
	var T = new Float64Array( 2 * 2 );
	dlarft( 'F', 'C', 6, 2, V, 1, 8, 0, TAU, 1, 0, T, 1, 2, 0 );
	// Verify T is populated (we just need the path to execute)
	// T[0] should be TAU[0] = 1.2
	if ( T[ 0 ] !== 1.2 ) {
		throw new Error( 'T[0,0] should be TAU[0]=1.2, got ' + T[ 0 ] );
	}
	// T[1,1] should be TAU[1] = 1.5
	if ( T[ 1 + 1 * 2 ] !== 1.5 ) {
		throw new Error( 'T[1,1] should be TAU[1]=1.5, got ' + T[ 1 + 1 * 2 ] );
	}
});

test( 'dlarft: fwd row with trailing zeros in V (exercises lastv assignment, lines 86-87)', function t() {
	// Row-wise storage with trailing zeros. V is K x N = 2 x 6 in Fortran.
	// V row 0: [1, 0.5, 0.25, 0.125, 0, 0] (trailing zeros in cols 4,5)
	var V = new Float64Array( 8 * 8 );
	V[ 0 + 0 * 8 ] = 1; V[ 0 + 1 * 8 ] = 0.5; V[ 0 + 2 * 8 ] = 0.25; V[ 0 + 3 * 8 ] = 0.125;
	V[ 0 + 4 * 8 ] = 0.0; V[ 0 + 5 * 8 ] = 0.0;
	V[ 1 + 1 * 8 ] = 1; V[ 1 + 2 * 8 ] = 0.5; V[ 1 + 3 * 8 ] = 0.25;
	V[ 1 + 4 * 8 ] = 0.125; V[ 1 + 5 * 8 ] = 0.0625;
	var TAU = new Float64Array( [ 1.2, 1.5 ] );
	var T = new Float64Array( 2 * 2 );
	dlarft( 'F', 'R', 6, 2, V, 1, 8, 0, TAU, 1, 0, T, 1, 2, 0 );
	if ( T[ 0 ] !== 1.2 ) {
		throw new Error( 'T[0,0] should be TAU[0]=1.2, got ' + T[ 0 ] );
	}
});

test( 'dlarft: bwd col with leading zeros in V (exercises lastv assignment, lines 133-137)', function t() {
	// Backward, column-wise. V(:,i) should have leading zeros to trigger lastv=jj+1 at line 136.
	// V is N x K = 6 x 2, with unit diag at rows N-K..N-1 = 4..5
	var V = new Float64Array( 8 * 2 );
	// Column 0: leading zeros in rows 0,1, then non-zero from row 2
	V[ 0 + 0 * 8 ] = 0.0;
	V[ 1 + 0 * 8 ] = 0.0;
	V[ 2 + 0 * 8 ] = 0.125;
	V[ 3 + 0 * 8 ] = 0.25;
	V[ 4 + 0 * 8 ] = 1.0; // unit diag at N-K+0 = 4
	// Column 1: leading zeros in rows 0,1,2
	V[ 0 + 1 * 8 ] = 0.0;
	V[ 1 + 1 * 8 ] = 0.0;
	V[ 2 + 1 * 8 ] = 0.0;
	V[ 3 + 1 * 8 ] = 0.0625;
	V[ 5 + 1 * 8 ] = 1.0; // unit diag at N-K+1 = 5
	var TAU = new Float64Array( [ 1.5, 0.9 ] );
	var T = new Float64Array( 2 * 2 );
	dlarft( 'B', 'C', 6, 2, V, 1, 8, 0, TAU, 1, 0, T, 1, 2, 0 );
	// T diagonal should hold TAU values
	if ( T[ 0 ] !== 1.5 ) {
		throw new Error( 'T[0,0] should be TAU[0]=1.5, got ' + T[ 0 ] );
	}
	if ( T[ 1 + 1 * 2 ] !== 0.9 ) {
		throw new Error( 'T[1,1] should be TAU[1]=0.9, got ' + T[ 1 + 1 * 2 ] );
	}
});

test( 'dlarft: bwd row with leading zeros in V (exercises lastv assignment, lines 155-159)', function t() {
	// Backward, row-wise. V(i,:) should have leading zeros.
	// V is K x N = 2 x 6. Unit diag at columns N-K..N-1 = 4..5
	var V = new Float64Array( 8 * 8 );
	// Row 0 (i=0): leading zeros in cols 0,1
	V[ 0 + 0 * 8 ] = 0.0; V[ 0 + 1 * 8 ] = 0.0;
	V[ 0 + 2 * 8 ] = 0.125; V[ 0 + 3 * 8 ] = 0.25;
	V[ 0 + 4 * 8 ] = 1.0; V[ 0 + 5 * 8 ] = 0.0;
	// Row 1 (i=1): leading zeros in cols 0,1,2
	V[ 1 + 0 * 8 ] = 0.0; V[ 1 + 1 * 8 ] = 0.0; V[ 1 + 2 * 8 ] = 0.0;
	V[ 1 + 3 * 8 ] = 0.0625;
	V[ 1 + 4 * 8 ] = 0.0; V[ 1 + 5 * 8 ] = 1.0;
	var TAU = new Float64Array( [ 1.5, 0.9 ] );
	var T = new Float64Array( 2 * 2 );
	dlarft( 'B', 'R', 6, 2, V, 1, 8, 0, TAU, 1, 0, T, 1, 2, 0 );
	if ( T[ 0 ] !== 1.5 ) {
		throw new Error( 'T[0,0] should be TAU[0]=1.5, got ' + T[ 0 ] );
	}
});

test( 'dlarft: bwd col K=1 (exercises line 179: prevlastv set, i=0 else branch)', function t() {
	// With K=1 in backward mode, i=0 is also i===K-1, so the i<K-1 branch is skipped.
	// But with K=2, the second iteration (i=0) hits the i>0 check at line 178.
	// We need i>0 to be false AND i<K-1 to be true for line 179.
	// Actually line 179 is prevlastv = Math.min(prevlastv, lastv) when i>0 inside the i<K-1 block.
	// So we need K>=3 to have i=1 where i>0 and i<K-1.
	var V = new Float64Array( 8 * 3 );
	// N=6, K=3: unit diag at rows 3,4,5
	// Col 0: some values
	V[ 0 + 0 * 8 ] = 0.5; V[ 1 + 0 * 8 ] = 0.25; V[ 2 + 0 * 8 ] = 0.125;
	V[ 3 + 0 * 8 ] = 1.0;
	// Col 1:
	V[ 0 + 1 * 8 ] = 0.3; V[ 1 + 1 * 8 ] = 0.15; V[ 2 + 1 * 8 ] = 0.075;
	V[ 4 + 1 * 8 ] = 1.0;
	// Col 2:
	V[ 0 + 2 * 8 ] = 0.2; V[ 1 + 2 * 8 ] = 0.1; V[ 2 + 2 * 8 ] = 0.05;
	V[ 5 + 2 * 8 ] = 1.0;
	var TAU = new Float64Array( [ 1.2, 0.8, 1.1 ] );
	var T = new Float64Array( 3 * 3 );
	dlarft( 'B', 'C', 6, 3, V, 1, 8, 0, TAU, 1, 0, T, 1, 3, 0 );
	// T diagonal should hold TAU values
	if ( T[ 0 ] !== 1.2 ) {
		throw new Error( 'T[0,0] should be 1.2, got ' + T[ 0 ] );
	}
	if ( T[ 1 + 1 * 3 ] !== 0.8 ) {
		throw new Error( 'T[1,1] should be 0.8, got ' + T[ 1 + 1 * 3 ] );
	}
	if ( T[ 2 + 2 * 3 ] !== 1.1 ) {
		throw new Error( 'T[2,2] should be 1.1, got ' + T[ 2 + 2 * 3 ] );
	}
});
