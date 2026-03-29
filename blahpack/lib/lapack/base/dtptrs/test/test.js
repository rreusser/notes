'use strict';

// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dtptrs = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dtptrs.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dtptrs is a function', function t() {
	assert.equal( typeof dtptrs, 'function' );
});

test( 'dtptrs: upper_no_trans', function t() {
	var info;
	var tc;
	var ap;
	var b;
	tc = findCase( 'upper_no_trans' );

	// A = [2 1 3; 0 4 5; 0 0 6], packed upper: [2, 1, 4, 3, 5, 6]
	ap = new Float64Array( [ 2.0, 1.0, 4.0, 3.0, 5.0, 6.0 ] );
	b = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	info = dtptrs( 'upper', 'no-transpose', 'non-unit', 3, 1, ap, 1, 0, b, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( b, tc.x, 1e-14, 'x' );
});

test( 'dtptrs: lower_no_trans', function t() {
	var info;
	var tc;
	var ap;
	var b;
	tc = findCase( 'lower_no_trans' );

	// L = [2 0 0; 1 4 0; 3 5 6], packed lower: [2, 1, 3, 4, 5, 6]
	ap = new Float64Array( [ 2.0, 1.0, 3.0, 4.0, 5.0, 6.0 ] );
	b = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	info = dtptrs( 'lower', 'no-transpose', 'non-unit', 3, 1, ap, 1, 0, b, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( b, tc.x, 1e-14, 'x' );
});

test( 'dtptrs: upper_trans', function t() {
	var info;
	var tc;
	var ap;
	var b;
	tc = findCase( 'upper_trans' );

	// A = [2 1 3; 0 4 5; 0 0 6], packed upper: [2, 1, 4, 3, 5, 6]
	ap = new Float64Array( [ 2.0, 1.0, 4.0, 3.0, 5.0, 6.0 ] );
	b = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	info = dtptrs( 'upper', 'transpose', 'non-unit', 3, 1, ap, 1, 0, b, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( b, tc.x, 1e-14, 'x' );
});

test( 'dtptrs: lower_trans', function t() {
	var info;
	var tc;
	var ap;
	var b;
	tc = findCase( 'lower_trans' );

	// L = [2 0 0; 1 4 0; 3 5 6], packed lower: [2, 1, 3, 4, 5, 6]
	ap = new Float64Array( [ 2.0, 1.0, 3.0, 4.0, 5.0, 6.0 ] );
	b = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	info = dtptrs( 'lower', 'transpose', 'non-unit', 3, 1, ap, 1, 0, b, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( b, tc.x, 1e-14, 'x' );
});

test( 'dtptrs: upper_unit_diag', function t() {
	var info;
	var tc;
	var ap;
	var b;
	tc = findCase( 'upper_unit_diag' );

	// Unit diagonal upper, packed: [1, 2, 1, 3, 4, 1]
	ap = new Float64Array( [ 1.0, 2.0, 1.0, 3.0, 4.0, 1.0 ] );
	b = new Float64Array( [ 10.0, 5.0, 1.0 ] );
	info = dtptrs( 'upper', 'no-transpose', 'unit', 3, 1, ap, 1, 0, b, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( b, tc.x, 1e-14, 'x' );
});

test( 'dtptrs: lower_unit_diag', function t() {
	var info;
	var tc;
	var ap;
	var b;
	tc = findCase( 'lower_unit_diag' );

	// Unit diagonal lower, packed: [1, 2, 3, 1, 4, 1]
	ap = new Float64Array( [ 1.0, 2.0, 3.0, 1.0, 4.0, 1.0 ] );
	b = new Float64Array( [ 10.0, 5.0, 1.0 ] );
	info = dtptrs( 'lower', 'no-transpose', 'unit', 3, 1, ap, 1, 0, b, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( b, tc.x, 1e-14, 'x' );
});

test( 'dtptrs: n_zero (quick return)', function t() {
	var info;
	var tc;
	var ap;
	var b;
	tc = findCase( 'n_zero' );
	ap = new Float64Array( [ 1.0 ] );
	b = new Float64Array( [ 99.0 ] );
	info = dtptrs( 'upper', 'no-transpose', 'non-unit', 0, 1, ap, 1, 0, b, 1, 1, 0 );
	assert.equal( info, tc.info );
});

test( 'dtptrs: n_one', function t() {
	var info;
	var tc;
	var ap;
	var b;
	tc = findCase( 'n_one' );
	ap = new Float64Array( [ 5.0 ] );
	b = new Float64Array( [ 15.0 ] );
	info = dtptrs( 'upper', 'no-transpose', 'non-unit', 1, 1, ap, 1, 0, b, 1, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( b, tc.x, 1e-14, 'x' );
});

test( 'dtptrs: singular_upper (info > 0)', function t() {
	var info;
	var tc;
	var ap;
	var b;
	tc = findCase( 'singular_upper' );

	// Upper packed: [2, 1, 0, 3, 5, 6] -> A(2,2) = 0
	ap = new Float64Array( [ 2.0, 1.0, 0.0, 3.0, 5.0, 6.0 ] );
	b = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	info = dtptrs( 'upper', 'no-transpose', 'non-unit', 3, 1, ap, 1, 0, b, 1, 3, 0 );
	assert.equal( info, tc.info );
});

test( 'dtptrs: singular_lower (info > 0)', function t() {
	var info;
	var tc;
	var ap;
	var b;
	tc = findCase( 'singular_lower' );

	// Lower packed: [0, 1, 3, 4, 5, 6] -> L(1,1) = 0
	ap = new Float64Array( [ 0.0, 1.0, 3.0, 4.0, 5.0, 6.0 ] );
	b = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	info = dtptrs( 'lower', 'no-transpose', 'non-unit', 3, 1, ap, 1, 0, b, 1, 3, 0 );
	assert.equal( info, tc.info );
});

test( 'dtptrs: singular_lower_last (info > 0)', function t() {
	var info;
	var tc;
	var ap;
	var b;
	tc = findCase( 'singular_lower_last' );

	// Lower packed: [2, 1, 3, 4, 5, 0] -> L(3,3) = 0
	ap = new Float64Array( [ 2.0, 1.0, 3.0, 4.0, 5.0, 0.0 ] );
	b = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	info = dtptrs( 'lower', 'no-transpose', 'non-unit', 3, 1, ap, 1, 0, b, 1, 3, 0 );
	assert.equal( info, tc.info );
});

test( 'dtptrs: multi_rhs', function t() {
	var info;
	var tc;
	var ap;
	var b;
	tc = findCase( 'multi_rhs' );

	// A = [2 1 3; 0 4 5; 0 0 6], packed upper: [2, 1, 4, 3, 5, 6]

	// B is 3x2, col-major: col1=[1,2,3], col2=[4,5,6]
	ap = new Float64Array( [ 2.0, 1.0, 4.0, 3.0, 5.0, 6.0 ] );
	b = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 ] );
	info = dtptrs( 'upper', 'no-transpose', 'non-unit', 3, 2, ap, 1, 0, b, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( b, tc.x, 1e-14, 'x' );
});

test( 'dtptrs: upper_conj_trans (same as transpose for real)', function t() {
	var info;
	var tc;
	var ap;
	var b;
	tc = findCase( 'upper_conj_trans' );

	// A = [2 1 3; 0 4 5; 0 0 6], packed upper: [2, 1, 4, 3, 5, 6]

	// For real d-prefix routines, conjugate-transpose = transpose
	ap = new Float64Array( [ 2.0, 1.0, 4.0, 3.0, 5.0, 6.0 ] );
	b = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	info = dtptrs( 'upper', 'transpose', 'non-unit', 3, 1, ap, 1, 0, b, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( b, tc.x, 1e-14, 'x' );
});

test( 'dtptrs: lower_4x4', function t() {
	var info;
	var tc;
	var ap;
	var b;
	tc = findCase( 'lower_4x4' );

	// L = [3 0 0 0; 1 2 0 0; 4 1 5 0; 2 3 1 4]

	// Packed lower: [3, 1, 4, 2, 2, 1, 3, 5, 1, 4]
	ap = new Float64Array( [ 3.0, 1.0, 4.0, 2.0, 2.0, 1.0, 3.0, 5.0, 1.0, 4.0 ] );
	b = new Float64Array( [ 10.0, 20.0, 30.0, 40.0 ] );
	info = dtptrs( 'lower', 'no-transpose', 'non-unit', 4, 1, ap, 1, 0, b, 1, 4, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( b, tc.x, 1e-14, 'x' );
});
