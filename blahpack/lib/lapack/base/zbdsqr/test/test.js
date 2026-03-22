

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var zbdsqr = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zbdsqr.jsonl' ), 'utf8' ).trim().split( '\n' );
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
* Creates a complex identity matrix of size n as interleaved Float64Array.
* Column-major: stride1 = 1 (row stride in complex elements), stride2 = n.
*/
function complexIdentity( n ) {
	var out = new Float64Array( 2 * n * n );
	var i;
	for ( i = 0; i < n; i++ ) {
		// Element (i,i): real part at offset 2*(i*1 + i*n) = 2*(i + i*n)
		out[ 2 * ( i + i * n ) ] = 1.0;
	}
	return out;
}


// TESTS //

test( 'zbdsqr: upper_4x4_values_only', function t() {
	var rwork = new Float64Array( 40 );
	var info;
	var tc = findCase( 'upper_4x4_values_only' );
	var d = new Float64Array( [ 4.0, 3.0, 2.0, 1.0 ] );
	var e = new Float64Array( [ 1.0, 1.0, 1.0 ] );
	var vt = new Float64Array( 0 );
	var u = new Float64Array( 0 );
	var c = new Float64Array( 0 );

	info = zbdsqr( 'U', 4, 0, 0, 0,
		d, 1, 0,
		e, 1, 0,
		vt, 1, 1, 0,
		u, 1, 1, 0,
		c, 1, 1, 0,
		rwork, 1, 0
	);

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( d, new Float64Array( tc.d ), 1e-14, 'd' );
});

test( 'zbdsqr: upper_3x3_with_vt', function t() {
	var rwork = new Float64Array( 40 );
	var info;
	var tc = findCase( 'upper_3x3_with_vt' );
	var d = new Float64Array( [ 3.0, 2.0, 1.0 ] );
	var e = new Float64Array( [ 0.5, 0.5 ] );
	var vt = complexIdentity( 3 );  // 3x3 complex identity, col-major
	var u = new Float64Array( 0 );
	var c = new Float64Array( 0 );

	// VT: strideVT1=1 (rows), strideVT2=3 (columns) in complex elements
	info = zbdsqr( 'U', 3, 3, 0, 0,
		d, 1, 0,
		e, 1, 0,
		vt, 1, 3, 0,
		u, 1, 1, 0,
		c, 1, 1, 0,
		rwork, 1, 0
	);

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( d, new Float64Array( tc.d ), 1e-14, 'd' );
	assertArrayClose( vt, new Float64Array( tc.vt ), 1e-14, 'vt' );
});

test( 'zbdsqr: upper_3x3_with_vt_and_u', function t() {
	var rwork = new Float64Array( 40 );
	var info;
	var tc = findCase( 'upper_3x3_with_vt_and_u' );
	var d = new Float64Array( [ 5.0, 3.0, 1.0 ] );
	var e = new Float64Array( [ 2.0, 1.0 ] );
	var vt = complexIdentity( 3 );
	var u = complexIdentity( 3 );
	var c = new Float64Array( 0 );

	info = zbdsqr( 'U', 3, 3, 3, 0,
		d, 1, 0,
		e, 1, 0,
		vt, 1, 3, 0,
		u, 1, 3, 0,
		c, 1, 1, 0,
		rwork, 1, 0
	);

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( d, new Float64Array( tc.d ), 1e-14, 'd' );
	assertArrayClose( vt, new Float64Array( tc.vt ), 1e-14, 'vt' );
	assertArrayClose( u, new Float64Array( tc.u ), 1e-14, 'u' );
});

test( 'zbdsqr: lower_3x3_values_only', function t() {
	var rwork = new Float64Array( 40 );
	var info;
	var tc = findCase( 'lower_3x3_values_only' );
	var d = new Float64Array( [ 4.0, 3.0, 2.0 ] );
	var e = new Float64Array( [ 1.5, 0.5 ] );
	var vt = new Float64Array( 0 );
	var u = new Float64Array( 0 );
	var c = new Float64Array( 0 );

	info = zbdsqr( 'L', 3, 0, 0, 0,
		d, 1, 0,
		e, 1, 0,
		vt, 1, 1, 0,
		u, 1, 1, 0,
		c, 1, 1, 0,
		rwork, 1, 0
	);

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( d, new Float64Array( tc.d ), 1e-14, 'd' );
});

test( 'zbdsqr: lower_3x3_with_u', function t() {
	var rwork = new Float64Array( 40 );
	var info;
	var tc = findCase( 'lower_3x3_with_u' );
	var d = new Float64Array( [ 4.0, 3.0, 2.0 ] );
	var e = new Float64Array( [ 1.5, 0.5 ] );
	var vt = new Float64Array( 0 );
	var u = complexIdentity( 3 );
	var c = new Float64Array( 0 );

	info = zbdsqr( 'L', 3, 0, 3, 0,
		d, 1, 0,
		e, 1, 0,
		vt, 1, 1, 0,
		u, 1, 3, 0,
		c, 1, 1, 0,
		rwork, 1, 0
	);

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( d, new Float64Array( tc.d ), 1e-14, 'd' );
	assertArrayClose( u, new Float64Array( tc.u ), 1e-14, 'u' );
});

test( 'zbdsqr: n_1', function t() {
	var rwork = new Float64Array( 10 );
	var info;
	var tc = findCase( 'n_1' );
	var d = new Float64Array( [ -5.0 ] );
	var e = new Float64Array( 0 );
	var vt = new Float64Array( 0 );
	var u = new Float64Array( 0 );
	var c = new Float64Array( 0 );

	info = zbdsqr( 'U', 1, 0, 0, 0,
		d, 1, 0,
		e, 1, 0,
		vt, 1, 1, 0,
		u, 1, 1, 0,
		c, 1, 1, 0,
		rwork, 1, 0
	);

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( d, new Float64Array( tc.d ), 1e-14, 'd' );
});

test( 'zbdsqr: n_0', function t() {
	var rwork = new Float64Array( 10 );
	var info;
	var tc = findCase( 'n_0' );
	var d = new Float64Array( 0 );
	var e = new Float64Array( 0 );
	var vt = new Float64Array( 0 );
	var u = new Float64Array( 0 );
	var c = new Float64Array( 0 );

	info = zbdsqr( 'U', 0, 0, 0, 0,
		d, 1, 0,
		e, 1, 0,
		vt, 1, 1, 0,
		u, 1, 1, 0,
		c, 1, 1, 0,
		rwork, 1, 0
	);

	assert.equal( info, tc.info, 'info' );
});

test( 'zbdsqr: upper_2x2_with_vectors', function t() {
	var rwork = new Float64Array( 40 );
	var info;
	var tc = findCase( 'upper_2x2_with_vectors' );
	var d = new Float64Array( [ 3.0, 1.0 ] );
	var e = new Float64Array( [ 2.0 ] );
	var vt = complexIdentity( 2 );
	var u = complexIdentity( 2 );
	var c = new Float64Array( 0 );

	info = zbdsqr( 'U', 2, 2, 2, 0,
		d, 1, 0,
		e, 1, 0,
		vt, 1, 2, 0,
		u, 1, 2, 0,
		c, 1, 1, 0,
		rwork, 1, 0
	);

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( d, new Float64Array( tc.d ), 1e-14, 'd' );
	assertArrayClose( vt, new Float64Array( tc.vt ), 1e-14, 'vt' );
	assertArrayClose( u, new Float64Array( tc.u ), 1e-14, 'u' );
});

test( 'zbdsqr: n_1_neg_with_vt', function t() {
	var rwork = new Float64Array( 10 );
	var info;
	var tc = findCase( 'n_1_neg_with_vt' );
	var d = new Float64Array( [ -3.0 ] );
	var e = new Float64Array( 0 );
	// VT is 1x2 complex matrix (1 row, 2 columns)
	// col-major: strideVT1=1, strideVT2=1 (both in complex elements)
	// Element (0,0) = (1, 2), element (0,1) = (3, 4)
	var vt = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
	var u = new Float64Array( 0 );
	var c = new Float64Array( 0 );

	info = zbdsqr( 'U', 1, 2, 0, 0,
		d, 1, 0,
		e, 1, 0,
		vt, 1, 1, 0,
		u, 1, 1, 0,
		c, 1, 1, 0,
		rwork, 1, 0
	);

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( d, new Float64Array( tc.d ), 1e-14, 'd' );
	assertArrayClose( vt, new Float64Array( tc.vt ), 1e-14, 'vt' );
});

test( 'zbdsqr: upper_3x3_with_c', function t() {
	var rwork = new Float64Array( 40 );
	var info;
	var tc = findCase( 'upper_3x3_with_c' );
	var d = new Float64Array( [ 4.0, 2.0, 1.0 ] );
	var e = new Float64Array( [ 1.0, 0.5 ] );
	var vt = new Float64Array( 0 );
	var u = new Float64Array( 0 );
	// C is 3x2 complex matrix (LDC=3)
	// Column-major: col 0 = [(1,0), (0,1), (1,1)], col 1 = [(2,0), (0,2), (2,2)]
	var c = new Float64Array( [
		1.0, 0.0,  0.0, 1.0,  1.0, 1.0,   // column 0: 3 complex elements
		2.0, 0.0,  0.0, 2.0,  2.0, 2.0    // column 1: 3 complex elements
	] );

	info = zbdsqr( 'U', 3, 0, 0, 2,
		d, 1, 0,
		e, 1, 0,
		vt, 1, 1, 0,
		u, 1, 1, 0,
		c, 1, 3, 0,
		rwork, 1, 0
	);

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( d, new Float64Array( tc.d ), 1e-14, 'd' );
	assertArrayClose( c, new Float64Array( tc.c ), 1e-14, 'c' );
});

test( 'zbdsqr: upper_4x4_idir2', function t() {
	var rwork = new Float64Array( 40 );
	var info;
	var tc = findCase( 'upper_4x4_idir2' );
	var d = new Float64Array( [ 0.5, 1.0, 2.0, 4.0 ] );
	var e = new Float64Array( [ 0.1, 0.1, 0.1 ] );
	var vt = new Float64Array( 0 );
	var u = new Float64Array( 0 );
	var c = new Float64Array( 0 );

	info = zbdsqr( 'U', 4, 0, 0, 0,
		d, 1, 0,
		e, 1, 0,
		vt, 1, 1, 0,
		u, 1, 1, 0,
		c, 1, 1, 0,
		rwork, 1, 0
	);

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( d, new Float64Array( tc.d ), 1e-14, 'd' );
});

test( 'zbdsqr: upper_3x3_zero_shift', function t() {
	var rwork = new Float64Array( 40 );
	var info;
	var tc = findCase( 'upper_3x3_zero_shift' );
	var d = new Float64Array( [ 1.0, 1e-15, 1.0 ] );
	var e = new Float64Array( [ 1.0, 1.0 ] );
	var vt = complexIdentity( 3 );
	var u = complexIdentity( 3 );
	var c = new Float64Array( 0 );

	info = zbdsqr( 'U', 3, 3, 3, 0,
		d, 1, 0,
		e, 1, 0,
		vt, 1, 3, 0,
		u, 1, 3, 0,
		c, 1, 1, 0,
		rwork, 1, 0
	);

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( d, new Float64Array( tc.d ), 1e-13, 'd' );
	assertArrayClose( vt, new Float64Array( tc.vt ), 1e-13, 'vt' );
	assertArrayClose( u, new Float64Array( tc.u ), 1e-13, 'u' );
});

test( 'zbdsqr: lower_3x3_with_c', function t() {
	var rwork = new Float64Array( 40 );
	var info;
	var tc = findCase( 'lower_3x3_with_c' );
	var d = new Float64Array( [ 3.0, 2.0, 1.0 ] );
	var e = new Float64Array( [ 0.5, 0.5 ] );
	var vt = new Float64Array( 0 );
	var u = new Float64Array( 0 );
	// C is 3x2 complex: col 0 = [(1,0), (0,0), (0,0)], col 1 = [(0,0), (1,0), (0,0)]
	var c = new Float64Array( [
		1.0, 0.0,  0.0, 0.0,  0.0, 0.0,
		0.0, 0.0,  1.0, 0.0,  0.0, 0.0
	] );

	info = zbdsqr( 'L', 3, 0, 0, 2,
		d, 1, 0,
		e, 1, 0,
		vt, 1, 1, 0,
		u, 1, 1, 0,
		c, 1, 3, 0,
		rwork, 1, 0
	);

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( d, new Float64Array( tc.d ), 1e-14, 'd' );
	assertArrayClose( c, new Float64Array( tc.c ), 1e-14, 'c' );
});

test( 'zbdsqr: upper_3x3_idir2_with_vectors', function t() {
	var rwork = new Float64Array( 40 );
	var info;
	var tc = findCase( 'upper_3x3_idir2_with_vectors' );
	var d = new Float64Array( [ 0.1, 0.5, 3.0 ] );
	var e = new Float64Array( [ 0.2, 0.3 ] );
	var vt = complexIdentity( 3 );
	var u = complexIdentity( 3 );
	var c = new Float64Array( 0 );

	info = zbdsqr( 'U', 3, 3, 3, 0,
		d, 1, 0,
		e, 1, 0,
		vt, 1, 3, 0,
		u, 1, 3, 0,
		c, 1, 1, 0,
		rwork, 1, 0
	);

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( d, new Float64Array( tc.d ), 1e-14, 'd' );
	assertArrayClose( vt, new Float64Array( tc.vt ), 1e-14, 'vt' );
	assertArrayClose( u, new Float64Array( tc.u ), 1e-14, 'u' );
});

test( 'zbdsqr: upper_3x3_negative_d', function t() {
	var rwork = new Float64Array( 40 );
	var info;
	var tc = findCase( 'upper_3x3_negative_d' );
	var d = new Float64Array( [ -3.0, 2.0, -1.0 ] );
	var e = new Float64Array( [ 0.5, 0.5 ] );
	var vt = complexIdentity( 3 );
	var u = new Float64Array( 0 );
	var c = new Float64Array( 0 );

	info = zbdsqr( 'U', 3, 3, 0, 0,
		d, 1, 0,
		e, 1, 0,
		vt, 1, 3, 0,
		u, 1, 1, 0,
		c, 1, 1, 0,
		rwork, 1, 0
	);

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( d, new Float64Array( tc.d ), 1e-14, 'd' );
	assertArrayClose( vt, new Float64Array( tc.vt ), 1e-14, 'vt' );
});

test( 'zbdsqr: nearly_diagonal', function t() {
	var rwork = new Float64Array( 40 );
	var info;
	var tc = findCase( 'nearly_diagonal' );
	var d = new Float64Array( [ 5.0, 3.0, 2.0, 1.0 ] );
	var e = new Float64Array( [ 1e-16, 1e-16, 1e-16 ] );
	var vt = new Float64Array( 0 );
	var u = new Float64Array( 0 );
	var c = new Float64Array( 0 );

	info = zbdsqr( 'U', 4, 0, 0, 0,
		d, 1, 0,
		e, 1, 0,
		vt, 1, 1, 0,
		u, 1, 1, 0,
		c, 1, 1, 0,
		rwork, 1, 0
	);

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( d, new Float64Array( tc.d ), 1e-14, 'd' );
});

test( 'zbdsqr: lower_3x3_with_vt_and_u', function t() {
	var rwork = new Float64Array( 40 );
	var info;
	var tc = findCase( 'lower_3x3_with_vt_and_u' );
	var d = new Float64Array( [ 3.0, 2.0, 1.0 ] );
	var e = new Float64Array( [ 0.5, 0.5 ] );
	var vt = complexIdentity( 3 );
	var u = complexIdentity( 3 );
	var c = new Float64Array( 0 );

	info = zbdsqr( 'L', 3, 3, 3, 0,
		d, 1, 0,
		e, 1, 0,
		vt, 1, 3, 0,
		u, 1, 3, 0,
		c, 1, 1, 0,
		rwork, 1, 0
	);

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( d, new Float64Array( tc.d ), 1e-14, 'd' );
	assertArrayClose( vt, new Float64Array( tc.vt ), 1e-14, 'vt' );
	assertArrayClose( u, new Float64Array( tc.u ), 1e-14, 'u' );
});
