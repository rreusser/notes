
'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dlasq4 = require( './../lib' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlasq4.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

/**
* Creates a Float64Array of given length filled with a value.
*/
function filled( len, val ) {
	var z = new Float64Array( len );
	var i;
	for ( i = 0; i < len; i++ ) {
		z[ i ] = val;
	}
	return z;
}

/**
* Checks result against fixture values.
*/
function checkResult( result, tc, tol ) {
	var relErr;
	// Check ttype (integer, exact)
	assert.strictEqual( result.ttype, tc.ttype, 'ttype mismatch: expected ' + tc.ttype + ', got ' + result.ttype );
	// Check tau
	if ( tc.tau === 0.0 ) {
		assert.ok( Math.abs( result.tau ) <= tol, 'tau: expected ~0, got ' + result.tau );
	} else {
		relErr = Math.abs( result.tau - tc.tau ) / Math.abs( tc.tau );
		assert.ok( relErr <= tol, 'tau: expected ' + tc.tau + ', got ' + result.tau + ' (relErr=' + relErr + ')' );
	}
	// Check g
	if ( tc.g === 0.0 ) {
		assert.ok( Math.abs( result.g ) <= tol, 'g: expected ~0, got ' + result.g );
	} else {
		relErr = Math.abs( result.g - tc.g ) / Math.abs( tc.g );
		assert.ok( relErr <= tol, 'g: expected ' + tc.g + ', got ' + result.g + ' (relErr=' + relErr + ')' );
	}
}


// TESTS //

test( 'dlasq4: main export is a function', function t() {
	assert.strictEqual( typeof dlasq4, 'function' );
});

test( 'dlasq4: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof dlasq4.ndarray, 'function' );
});

test( 'dlasq4: dmin_negative (ttype=-1)', function t() {
	var tc = findCase( 'dmin_negative' );
	var z = filled( 100, 1.0 );
	var result = dlasq4.ndarray( 1, 5, z, 1, 0, 0, 5, -0.5, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0, 0.0 );
	checkResult( result, tc, 1e-14 );
});

test( 'dlasq4: dmin_zero (ttype=-1)', function t() {
	var tc = findCase( 'dmin_zero' );
	var z = filled( 100, 1.0 );
	var result = dlasq4.ndarray( 1, 5, z, 1, 0, 0, 5, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0, 0.0 );
	checkResult( result, tc, 1e-14 );
});

test( 'dlasq4: case2_ttype_neg2 — Cases 2/3 with gap analysis', function t() {
	var tc = findCase( 'case2_ttype_neg2' );
	var z = filled( 100, 4.0 );
	// NN=4*5+0=20; Fortran indices: Z(17),Z(15),Z(13),Z(11)
	z[ 16 ] = 1.0; // Z(17)
	z[ 14 ] = 1.0; // Z(15)
	z[ 12 ] = 1.0; // Z(13)
	z[ 10 ] = 1.0; // Z(11)
	var result = dlasq4.ndarray( 1, 5, z, 1, 0, 0, 5, 0.5, 0.5, 3.0, 0.5, 2.0, 3.0, 0.0, 0, 0.0 );
	checkResult( result, tc, 1e-14 );
});

test( 'dlasq4: case3_ttype_neg3 — Case 3, gap1 <= 0', function t() {
	var tc = findCase( 'case3_ttype_neg3' );
	var z = filled( 100, 4.0 );
	z[ 16 ] = 2.0;
	z[ 14 ] = 2.0;
	z[ 12 ] = 2.0;
	z[ 10 ] = 2.0;
	var result = dlasq4.ndarray( 1, 5, z, 1, 0, 0, 5, 3.5, 3.5, 4.5, 3.5, 3.5, 4.5, 0.0, 0, 0.0 );
	checkResult( result, tc, 1e-14 );
});

test( 'dlasq4: case4_dmin_eq_dn — Case 4 with dmin=dn', function t() {
	var tc = findCase( 'case4_dmin_eq_dn' );
	var z = filled( 100, 4.0 );
	z[ 16 ] = 1.0; z[ 14 ] = 1.0; z[ 12 ] = 2.0; z[ 10 ] = 1.0;
	z[ 8 ] = 1.0; z[ 6 ] = 1.5; z[ 4 ] = 1.0; z[ 2 ] = 1.2;
	var result = dlasq4.ndarray( 1, 5, z, 1, 0, 0, 5, 0.5, 1.5, 2.0, 0.5, 1.0, 2.0, 0.0, 0, 0.0 );
	checkResult( result, tc, 1e-14 );
});

test( 'dlasq4: case4_dmin_eq_dn1 — Case 4 with dmin=dn1', function t() {
	var tc = findCase( 'case4_dmin_eq_dn1' );
	var z = filled( 100, 4.0 );
	z[ 19 ] = 1.0; z[ 17 ] = 1.5; z[ 15 ] = 1.0; z[ 13 ] = 2.0;
	z[ 11 ] = 1.0; z[ 10 ] = 1.0; z[ 8 ] = 1.0; z[ 6 ] = 1.5;
	z[ 4 ] = 1.0; z[ 2 ] = 1.2;
	var result = dlasq4.ndarray( 1, 5, z, 1, 0, 0, 5, 0.5, 1.5, 2.0, 1.0, 0.5, 2.0, 0.0, 0, 0.0 );
	checkResult( result, tc, 1e-14 );
});

test( 'dlasq4: case5_dmin_eq_dn2 — Case 5', function t() {
	var tc = findCase( 'case5_dmin_eq_dn2' );
	var z = filled( 100, 4.0 );
	z[ 19 ] = 1.0; z[ 17 ] = 1.0; z[ 15 ] = 1.0; z[ 14 ] = 1.0;
	z[ 13 ] = 1.0; z[ 11 ] = 1.0; z[ 9 ] = 1.0; z[ 7 ] = 1.5;
	z[ 5 ] = 1.0; z[ 4 ] = 1.0; z[ 3 ] = 1.0; z[ 2 ] = 1.0;
	z[ 1 ] = 1.0; z[ 0 ] = 1.0;
	var result = dlasq4.ndarray( 1, 5, z, 1, 0, 0, 5, 0.5, 1.5, 1.0, 1.5, 2.0, 0.5, 0.0, 0, 0.0 );
	checkResult( result, tc, 1e-14 );
});

test( 'dlasq4: case6_no_info — Case 6, fresh g', function t() {
	var tc = findCase( 'case6_no_info' );
	var z = filled( 100, 4.0 );
	var result = dlasq4.ndarray( 1, 5, z, 1, 0, 0, 5, 1.0, 2.0, 3.0, 2.0, 3.0, 4.0, 0.0, 0, 0.0 );
	checkResult( result, tc, 1e-14 );
});

test( 'dlasq4: case6_ttype_neg6 — Case 6, previous ttype=-6 updates g', function t() {
	var tc = findCase( 'case6_ttype_neg6' );
	var z = filled( 100, 4.0 );
	var result = dlasq4.ndarray( 1, 5, z, 1, 0, 0, 5, 1.0, 2.0, 3.0, 2.0, 3.0, 4.0, 0.0, -6, 0.5 );
	checkResult( result, tc, 1e-14 );
});

test( 'dlasq4: case6_ttype_neg18 — Case 6, previous ttype=-18 resets g', function t() {
	var tc = findCase( 'case6_ttype_neg18' );
	var z = filled( 100, 4.0 );
	var result = dlasq4.ndarray( 1, 5, z, 1, 0, 0, 5, 1.0, 2.0, 3.0, 2.0, 3.0, 4.0, 0.0, -18, 0.5 );
	checkResult( result, tc, 1e-14 );
});

test( 'dlasq4: case7_one_deflated — Case 7 (n0in=n0+1)', function t() {
	var tc = findCase( 'case7_one_deflated' );
	var z = filled( 100, 4.0 );
	z[ 16 ] = 1.0; z[ 14 ] = 2.0; z[ 12 ] = 1.0; z[ 10 ] = 1.5;
	z[ 8 ] = 1.0; z[ 6 ] = 2.0; z[ 4 ] = 1.0; z[ 2 ] = 1.5;
	var result = dlasq4.ndarray( 1, 5, z, 1, 0, 0, 6, 0.8, 0.8, 1.5, 1.0, 0.8, 1.5, 0.0, 0, 0.0 );
	checkResult( result, tc, 1e-14 );
});

test( 'dlasq4: case9_dmin1_ne_dn1 — Case 9 (dmin1 != dn1)', function t() {
	var tc = findCase( 'case9_dmin1_ne_dn1' );
	var z = filled( 100, 4.0 );
	var result = dlasq4.ndarray( 1, 5, z, 1, 0, 0, 6, 0.8, 0.9, 1.5, 1.0, 0.8, 1.5, 0.0, 0, 0.0 );
	checkResult( result, tc, 1e-14 );
});

test( 'dlasq4: case9_dmin1_eq_dn1 — Case 9 (dmin1=dn1, s=half*dmin1)', function t() {
	var tc = findCase( 'case9_dmin1_eq_dn1' );
	var z = filled( 100, 4.0 );
	var result = dlasq4.ndarray( 1, 5, z, 1, 0, 0, 6, 0.8, 0.8, 1.6, 1.0, 0.8, 1.5, 0.0, 0, 0.0 );
	checkResult( result, tc, 1e-14 );
});

test( 'dlasq4: case10_two_deflated — Case 10 (n0in=n0+2)', function t() {
	var tc = findCase( 'case10_two_deflated' );
	var z = filled( 100, 4.0 );
	z[ 14 ] = 0.3; z[ 12 ] = 2.0; z[ 10 ] = 1.0; z[ 8 ] = 1.0;
	z[ 6 ] = 1.5; z[ 4 ] = 1.0; z[ 2 ] = 1.2;
	var result = dlasq4.ndarray( 1, 5, z, 1, 0, 0, 7, 0.5, 0.8, 0.5, 1.0, 0.8, 0.5, 0.0, 0, 0.0 );
	checkResult( result, tc, 1e-14 );
});

test( 'dlasq4: case11_two_deflated_fallback — Case 11 (ttype=-11)', function t() {
	var tc = findCase( 'case11_two_deflated_fallback' );
	var z = filled( 100, 4.0 );
	z[ 14 ] = 2.0; z[ 12 ] = 4.0;
	var result = dlasq4.ndarray( 1, 5, z, 1, 0, 0, 7, 0.5, 0.8, 0.5, 1.0, 0.8, 0.5, 0.0, 0, 0.0 );
	checkResult( result, tc, 1e-14 );
});

test( 'dlasq4: case12_many_deflated — Case 12 (n0in>n0+2)', function t() {
	var tc = findCase( 'case12_many_deflated' );
	var z = filled( 100, 4.0 );
	var result = dlasq4.ndarray( 1, 5, z, 1, 0, 0, 8, 0.5, 0.8, 0.5, 1.0, 0.8, 0.5, 0.0, 0, 0.0 );
	checkResult( result, tc, 1e-14 );
});

test( 'dlasq4: pp1_case2 — PP=1 (pong)', function t() {
	var tc = findCase( 'pp1_case2' );
	var z = filled( 100, 4.0 );
	// NN=4*5+1=21; Fortran Z(18),Z(16),Z(14),Z(12) -> JS z[17],z[15],z[13],z[11]
	z[ 17 ] = 1.0; z[ 15 ] = 1.0; z[ 13 ] = 1.0; z[ 11 ] = 1.0;
	var result = dlasq4.ndarray( 1, 5, z, 1, 0, 1, 5, 0.5, 0.5, 3.0, 0.5, 2.0, 3.0, 0.0, 0, 0.0 );
	checkResult( result, tc, 1e-14 );
});

test( 'dlasq4: case4_early_return — Z(NN-5) > Z(NN-7) triggers early return', function t() {
	var tc = findCase( 'case4_early_return' );
	var z = filled( 100, 4.0 );
	z[ 16 ] = 1.0; z[ 14 ] = 5.0; z[ 12 ] = 2.0; z[ 10 ] = 1.0;
	var result = dlasq4.ndarray( 1, 5, z, 1, 0, 0, 5, 0.5, 1.5, 2.0, 0.5, 1.0, 2.0, 0.0, 0, 0.0 );
	checkResult( result, tc, 1e-14 );
});

test( 'dlasq4: case5_early_return — Z(NP-8) > B2', function t() {
	var tc = findCase( 'case5_early_return' );
	var z = filled( 100, 4.0 );
	// NN=20, NP=20; Z(18)=z[17], Z(14)=z[13], Z(12)=z[11], Z(16)=z[15]
	z[ 17 ] = 1.0;  // b1=Z(NP-2)
	z[ 13 ] = 0.5;  // b2=Z(NP-6)
	z[ 11 ] = 10.0; // Z(NP-8)>b2 triggers return
	z[ 15 ] = 0.5;
	var result = dlasq4.ndarray( 1, 5, z, 1, 0, 0, 5, 0.5, 1.5, 1.0, 1.5, 2.0, 0.5, 0.0, 0, 0.0 );
	checkResult( result, tc, 1e-14 );
});

test( 'dlasq4: case3_dn_gt_b1 — Case 3 with dn > b1', function t() {
	var tc = findCase( 'case3_dn_gt_b1' );
	var z = filled( 100, 4.0 );
	z[ 16 ] = 0.01; z[ 14 ] = 0.01; z[ 12 ] = 0.01; z[ 10 ] = 0.01;
	var result = dlasq4.ndarray( 1, 5, z, 1, 0, 0, 5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.0, 0, 0.0 );
	checkResult( result, tc, 1e-14 );
});

test( 'dlasq4: case7_b2_zero — b2=0 early exit', function t() {
	var tc = findCase( 'case7_b2_zero' );
	var z = filled( 100, 4.0 );
	z[ 14 ] = 0.0; z[ 12 ] = 2.0;
	var result = dlasq4.ndarray( 1, 5, z, 1, 0, 0, 6, 0.8, 0.8, 1.5, 1.0, 0.8, 1.5, 0.0, 0, 0.0 );
	checkResult( result, tc, 1e-14 );
});

test( 'dlasq4: case8_gap2_negative — ttype=-7/8 with gap2<=0', function t() {
	var tc = findCase( 'case8_gap2_negative' );
	var z = filled( 100, 4.0 );
	z[ 16 ] = 1.0; z[ 14 ] = 1.0; z[ 12 ] = 2.0; z[ 10 ] = 1.5;
	z[ 8 ] = 1.0; z[ 6 ] = 2.0; z[ 4 ] = 1.0; z[ 2 ] = 1.5;
	var result = dlasq4.ndarray( 1, 5, z, 1, 0, 0, 6, 0.1, 0.8, 0.1, 1.0, 0.8, 0.1, 0.0, 0, 0.0 );
	checkResult( result, tc, 1e-14 );
});

test( 'dlasq4: case10_b2_zero — Case 10 with b2=0', function t() {
	var tc = findCase( 'case10_b2_zero' );
	var z = filled( 100, 4.0 );
	z[ 14 ] = 0.0; z[ 12 ] = 2.0; z[ 10 ] = 1.0; z[ 8 ] = 1.0;
	var result = dlasq4.ndarray( 1, 5, z, 1, 0, 0, 7, 0.5, 0.8, 0.5, 1.0, 0.8, 0.5, 0.0, 0, 0.0 );
	checkResult( result, tc, 1e-14 );
});

test( 'dlasq4: case2_gap2_neg — Case 2 with gap2<=0', function t() {
	var tc = findCase( 'case2_gap2_neg' );
	var z = filled( 100, 4.0 );
	z[ 16 ] = 1.0; z[ 14 ] = 1.0; z[ 12 ] = 1.0; z[ 10 ] = 1.0;
	var result = dlasq4.ndarray( 1, 5, z, 1, 0, 0, 5, 0.5, 0.5, 1.0, 0.5, 0.5, 1.0, 0.0, 0, 0.0 );
	checkResult( result, tc, 1e-14 );
});

test( 'dlasq4: case5_short_array — Case 5 with n0-i0<=2', function t() {
	var tc = findCase( 'case5_short_array' );
	var z = filled( 100, 4.0 );
	z[ 17 ] = 1.0; z[ 13 ] = 0.5; z[ 11 ] = 0.3; z[ 15 ] = 0.5;
	var result = dlasq4.ndarray( 3, 5, z, 1, 0, 0, 5, 0.5, 1.5, 1.0, 1.5, 2.0, 0.5, 0.0, 0, 0.0 );
	checkResult( result, tc, 1e-14 );
});
